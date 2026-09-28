import importlib.util, json, os, sqlite3, subprocess, tempfile, textwrap, unittest
from pathlib import Path

HERE = Path(__file__).parent
spec = importlib.util.spec_from_file_location("worker", HERE / "warrant_rerun_worker.py")
w = importlib.util.module_from_spec(spec); spec.loader.exec_module(w)

class WorkerTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(); self.root = Path(self.tmp.name)
        self.repo = self.root / "repo"; self.repo.mkdir()
        subprocess.run(["git", "init", "-q"], cwd=self.repo, check=True)
        subprocess.run(["git", "config", "user.email", "test@example.org"], cwd=self.repo, check=True)
        subprocess.run(["git", "config", "user.name", "Test"], cwd=self.repo, check=True)
        path = self.repo / "test/example_test.clj"; path.parent.mkdir(); path.write_text("ok\n")
        subprocess.run(["git", "add", "."], cwd=self.repo, check=True); subprocess.run(["git", "commit", "-qm", "init"], cwd=self.repo, check=True)
        self.db = str(self.root / "db.sqlite"); w.REPOS = {"futon3c": str(self.repo)}
        with w.connect(self.db) as db:
            db.executescript("""CREATE TABLE registry_entries(id TEXT PRIMARY KEY,payload_text TEXT,payload_sha TEXT);
            CREATE TABLE registry_runs(entry_id TEXT PRIMARY KEY,repo_root TEXT,namespace TEXT,command_key TEXT,
            ran_at TEXT,finished_at TEXT,warrant INTEGER,revision TEXT,ran_order TEXT,finished_order TEXT);""")
        self.calls = self.root / "calls"; self.envout = self.root / "env"
        self.runner = self.root / "runner.py"
        self.runner.write_text(textwrap.dedent(f"""
            #!/usr/bin/env python3
            import hashlib,json,os,sqlite3,subprocess,sys,time
            ns=sys.argv[-1]; root=os.getcwd(); head=sys.argv[-2]; db=sqlite3.connect(os.environ['REGISTRY_DB'])
            open({str(self.calls)!r},'a').write(ns+' start\\n'); open({str(self.envout)!r},'a').write(str(os.environ.get('GIT_DIR'))+'\\n')
            time.sleep(.08); now='9999-01-01T00:00:00Z'; eid='entry-'+ns
            payload=json.dumps({{'warrant?':True,'load-closure':[],'test-files':{{'test/example_test.clj':'0'*64}},'repo/root':root}})
            db.execute('INSERT OR REPLACE INTO registry_entries VALUES(?,?,?)',(eid,payload,hashlib.sha256(payload.encode()).hexdigest()))
            db.execute('INSERT OR REPLACE INTO registry_runs VALUES(?,?,?,?,?,?,?,?,?,?)',(eid,root,ns,'c',now,now,1,head,now,now)); db.commit()
            open({str(self.calls)!r},'a').write(ns+' end\\n')
            """).lstrip()); self.runner.chmod(0o755)
    def tearDown(self): self.tmp.cleanup()
    def queue(self, ns="example-test"):
        with w.connect(self.db) as db:
            return db.execute("INSERT INTO warrant_rerun_requests(namespace,repo,reason,requested_at,state) VALUES(?,?,?,?, 'queued') RETURNING request_id",
                              (ns,"futon3c","absent","2020-01-01T00:00:00Z")).fetchone()[0]
    def args(self, parallel=1):
        return type("A",(),dict(db=self.db,parallel=parallel,runner=str(self.runner),log_dir=str(self.root/"logs")))()
    def test_success_and_scrubbed_environment(self):
        rid=self.queue(); os.environ["GIT_DIR"]="poison"
        try: w.run_pass(self.args())
        finally: os.environ.pop("GIT_DIR")
        with w.connect(self.db) as db: row=db.execute("SELECT state,entry_id FROM warrant_rerun_requests WHERE request_id=?",(rid,)).fetchone()
        self.assertEqual(("done","entry-example-test"),row); self.assertEqual("None",self.envout.read_text().strip())
        self.assertEqual(1,self.calls.read_text().count("start"))
    def test_done_writes_a_dependency_record_and_other_states_do_not(self):
        seen=[]; original=w.write_reach_record
        w.write_reach_record=lambda db,ns,d: seen.append((ns,d)) or '{"skipped": "stub"}'
        try:
            args=self.args(); args.reach_dir=str(self.root/"reach")
            rid=self.queue(); w.run_pass(args)
            with w.connect(self.db) as db: row=db.execute("SELECT state,detail FROM warrant_rerun_requests WHERE request_id=?",(rid,)).fetchone()
            self.assertEqual(("done",'reach: {"skipped": "stub"}'),row)
            self.assertEqual([("example-test",str(self.root/"reach"))],seen)
            self.runner.write_text("#!/bin/sh\nexit 7\n"); rid=self.queue("other-test"); w.run_pass(args)
            with w.connect(self.db) as db: self.assertEqual("failed",db.execute("SELECT state FROM warrant_rerun_requests WHERE request_id=?",(rid,)).fetchone()[0])
            self.assertEqual(1,len(seen))
        finally: w.write_reach_record=original
    def test_a_failing_record_step_leaves_the_request_done(self):
        args=self.args(); args.reach_dir=str(self.root/"reach")
        rid=self.queue(); w.run_pass(args)  # the real step, against the fixture's reduced schema
        with w.connect(self.db) as db: row=db.execute("SELECT state,detail FROM warrant_rerun_requests WHERE request_id=?",(rid,)).fetchone()
        self.assertEqual("done",row[0]); self.assertTrue(row[1].startswith("reach: "))
    def test_failure_without_run(self):
        self.runner.write_text("#!/bin/sh\nexit 1\n"); self.runner.chmod(0o755); rid=self.queue(); w.run_pass(self.args())
        with w.connect(self.db) as db: row=db.execute("SELECT state,detail FROM warrant_rerun_requests WHERE request_id=?",(rid,)).fetchone()
        self.assertEqual("failed",row[0]); self.assertIn("exit 1",row[1])
    def test_refusal_for_a_file_edited_during_the_run_holds(self):
        self.runner.write_text(self.runner.read_text()
                               .replace("'warrant?':True", "'warrant?':False,'postcheck':':reason :scope-not-committed'")
                               .replace("(eid,root,ns,'c',now,now,1,head,now,now)", "(eid,root,ns,'c',now,now,0,head,now,now)"))
        rid=self.queue(); w.run_pass(self.args())
        with w.connect(self.db) as db: row=db.execute("SELECT state,detail,entry_id FROM warrant_rerun_requests WHERE request_id=?",(rid,)).fetchone()
        self.assertEqual("queued",row[0]); self.assertIn("scope-not-committed",row[1]); self.assertEqual("entry-example-test",row[2])
        self.assertEqual(1,self.calls.read_text().count("start"))
    def test_refusal_for_another_reason_fails(self):
        self.runner.write_text(self.runner.read_text()
                               .replace("'warrant?':True", "'warrant?':False,'postcheck':':reason :tests-failed'")
                               .replace("(eid,root,ns,'c',now,now,1,head,now,now)", "(eid,root,ns,'c',now,now,0,head,now,now)"))
        rid=self.queue(); w.run_pass(self.args())
        with w.connect(self.db) as db: row=db.execute("SELECT state FROM warrant_rerun_requests WHERE request_id=?",(rid,)).fetchone()
        self.assertEqual("failed",row[0])
    def test_dirty_prior_closure_holds(self):
        head=subprocess.check_output(["git","rev-parse","HEAD"],cwd=self.repo,text=True).strip()
        payload='{:load-closure [] :test-files {"test/example_test.clj" "' + '0'*64 + '"} :repo/root "' + str(self.repo) + '"}'
        with w.connect(self.db) as db:
            db.execute("INSERT INTO registry_entries VALUES('old',?, 'x')",(payload,)); db.execute("INSERT INTO registry_runs VALUES('old',?,?,?,?,?,?,?,?,?)",(str(self.repo),'example-test','c','2020','2020',1,head,'2020','2020'))
        (self.repo/'test/example_test.clj').write_text('dirty\n'); rid=self.queue(); w.run_pass(self.args())
        with w.connect(self.db) as db: row=db.execute("SELECT state,detail FROM warrant_rerun_requests WHERE request_id=?",(rid,)).fetchone()
        self.assertEqual('queued',row[0]); self.assertIn('test/example_test.clj',row[1]); self.assertFalse(self.calls.exists())
    def test_parallel_batches_finish_three(self):
        for ns in ('a-test','b-test','c-test'): self.queue(ns)
        w.run_pass(self.args(2))
        with w.connect(self.db) as db: self.assertEqual([('done',3)],db.execute("SELECT state,count(*) FROM warrant_rerun_requests GROUP BY state").fetchall())
        events=self.calls.read_text().splitlines()
        self.assertTrue(events[0].endswith('start') and events[1].endswith('start'), events)

if __name__ == '__main__': unittest.main()
