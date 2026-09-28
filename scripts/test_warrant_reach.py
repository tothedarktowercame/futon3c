import importlib.util
import json
from pathlib import Path
import sys
import tempfile
import unittest


sys.path.insert(0, str(Path(__file__).parent))
SPEC = importlib.util.spec_from_file_location(
    'warrant_reach', Path(__file__).with_name('warrant_reach.py'))
reach = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(reach)


class ReachRecordTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        self.src = self.root / 'src' / 'sample'
        self.test = self.root / 'test' / 'sample'
        self.src.mkdir(parents=True)
        self.test.mkdir(parents=True)
        self.product = self.src / 'core.clj'
        self.test_file = self.test / 'core_test.clj'
        self.product.write_text('(ns sample.core)\n(defn reached [] 1)\n(defn spare [] 2)\n')
        self.test_file.write_text(
            '(ns sample.core-test (:require [sample.core :as core]))\n'
            '(defn exercise [] (core/reached))\n')
        self.closure = self.root / 'closure.edn'
        self._closure([self.product, self.test_file])
        self.record = self.root / 'record.json'
        self.cache = self.root / 'cache'
        self._record()

    def tearDown(self):
        self.temp.cleanup()

    def _closure(self, paths):
        rows = ' '.join('{:ns "loaded" :url "file:%s" :status :loaded-only}' % p
                        for p in paths)
        self.closure.write_text('[' + rows + ']')

    def _record(self):
        value = reach.qualify_record(reach.analyze(
            'sample.core-test', self.closure, cache_path=self.cache))
        self.record.write_text(json.dumps(value))

    def kinds(self):
        return {item['kind'] for item in reach.check_record(self.record, self.cache)}

    def test_reached_body_edit_is_definition_changed(self):
        self.product.write_text(self.product.read_text().replace('reached [] 1', 'reached [] 9'))
        self.assertIn('definition-changed', self.kinds())

    def test_unreached_body_edit_is_current(self):
        self.product.write_text(self.product.read_text().replace('spare [] 2', 'spare [] 9'))
        self.assertEqual([], reach.check_record(self.record, self.cache))

    def test_moving_definition_without_text_change_is_current(self):
        self.product.write_text('(ns sample.core)\n\n\n\n\n\n\n\n\n\n\n'
                                '(defn reached [] 1)\n(defn spare [] 2)\n')
        self.assertEqual([], reach.check_record(self.record, self.cache))

    def test_new_method_for_reached_multimethod_is_added(self):
        self.product.write_text('(ns sample.core)\n(defmulti reached identity)\n'
                                '(defmethod reached :old [_] 1)\n')
        methods = self.src / 'methods.clj'
        methods.write_text('(ns sample.methods (:require [sample.core :as core]))\n')
        self._closure([self.product, methods, self.test_file])
        self._record()
        methods.write_text(methods.read_text() + '(defmethod core/reached :new [_] 2)\n')
        self.assertIn('definition-added', self.kinds())

    def test_new_reference_names_changed_definition_and_added_target(self):
        self.product.write_text(self.product.read_text().replace(
            '(defn reached [] 1)', '(defn target [] 3)\n(defn reached [] (target))'))
        self.assertEqual({'definition-changed', 'definition-added'}, self.kinds())

    def test_top_level_side_effect_changes_remainder(self):
        self.product.write_text(self.product.read_text() + "(alter-var-root #'spare identity)\n")
        self.assertIn('remainder-changed', self.kinds())

    def test_reached_constant_edit_is_definition_changed(self):
        self.product.write_text('(ns sample.core)\n(def reached 1)\n(defn spare [] 2)\n')
        self._record()
        self.product.write_text(self.product.read_text().replace('(def reached 1)', '(def reached 2)'))
        self.assertIn('definition-changed', self.kinds())

    def test_analysis_error_file_is_whole(self):
        self.product.write_text('(ns sample.core)\n(defn reached [] (unknown x))\n')
        self._record()
        self.product.write_text(self.product.read_text().replace('unknown x', 'unknown y'))
        self.assertIn('whole-file-changed', self.kinds())

    def test_top_level_form_acting_on_a_reached_definition_elsewhere(self):
        other = self.src / 'other.clj'
        other.write_text('(ns sample.other (:require [sample.core]))\n(defn unused [] 0)\n')
        self._closure([self.product, other, self.test_file])
        self._record()
        self.assertEqual([], reach.check_record(self.record, self.cache))
        other.write_text(other.read_text()
                         + "(alter-var-root #'sample.core/reached (constantly (fn [] 7)))\n")
        self.assertEqual({'remainder-added'}, self.kinds())
        self._record()
        other.write_text(other.read_text().replace('(fn [] 7)', '(fn [] 8)'))
        self.assertEqual({'remainder-changed'}, self.kinds())

    def test_called_definition_is_an_additional_root(self):
        called = self.root / 'called.edn'
        called.write_text('[{:ns "sample.core" :name "spare"}]')
        value = reach.analyze('sample.core-test', self.closure, called, self.cache)
        names = {(item['ns'], item['name']) for item in value['reached-definitions']}
        self.assertIn(('sample.core', 'spare'), names)


if __name__ == '__main__':
    unittest.main()
