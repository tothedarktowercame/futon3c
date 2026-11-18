'use strict';

// Evaluator over finite, ground query results. XTQL remains the authority;
// this module neither queries nor writes a store and does not infer completeness.
const { Rel, run, eq, conde, succeedo, failo } = require('ramo');

const membero = (value, values) => conde(...values.map(v => eq(value, v)));

// Only closed goals are passed here: no variable from an outer query is used.
// One witness suffices to refute absence. Zero witnesses requires exhausting the
// finite ground relation. This is not a general nonground negation operator.
function groundNot(goal) {
  return run(1)(q => [goal, eq(q, true)]).length ? failo : succeedo;
}

const uncovered = Rel((q, facts) => conde(...facts.citations.map(feature => [
  groundNot(membero(feature, facts.documented)), eq(q, feature)
])));

const invalidFunctions = Rel((q, facts) => {
  const valid = facts.rules.map(row => row[1]).filter(name => typeof name === 'string' && name.length > 0);
  const missing = facts.rules.map(([id, name]) => [
    groundNot(membero(name, valid)), eq(q, { rule: id, reason: 'missing-qualname' })
  ]);
  const duplicates = facts.rules.flatMap(([a, name]) => facts.rules.map(([b, other]) => [
    groundNot(eq(a, b)), eq(name, other), eq(q, { 'rule-a': a, 'rule-b': b, qualname: name })
  ]));
  return conde(...missing, ...duplicates);
});

const invalidLinks = Rel((q, facts) => conde(...facts.links.flatMap(([id, type, from, to]) => {
  const ports = {
    'design/checked-by': ['check', facts.checks],
    'design/shaped-by': ['ruling', facts.rulings],
    'design/constrains': ['target', facts.refs]
  };
  if (!Object.hasOwn(ports, type)) throw new Error(`Unknown catalogue link type: ${type}`);
  const [role, allowed] = ports[type];
  return [
    [groundNot(membero(from, facts.choices)), eq(q, { link: id, role: 'choice', target: from })],
    [groundNot(membero(to, allowed)), eq(q, { link: id, role, target: to })]
  ];
})));

function canonical(value) {
  if (Array.isArray(value)) return value.map(canonical);
  if (value !== null && typeof value === 'object') {
    return Object.fromEntries(Object.keys(value).sort().map(k => [k, canonical(value[k])]));
  }
  return value;
}
function violationSet(violations) {
  return [...new Set(violations.map(v => JSON.stringify(canonical(v))))].sort();
}

function check(fixture) {
  const relations = { coverage: uncovered, 'rule-function': invalidFunctions, links: invalidLinks };
  if (!Object.hasOwn(relations, fixture.check)) throw new Error(`Unknown catalogue check: ${fixture.check}`);
  // run() is exhaustive; no result limit/timeout silently returns a partial pass.
  // All relations enumerate finite input arrays. Errors propagate.
  const violations = run()(q => relations[fixture.check](q, fixture.facts));
  const unique = violationSet(violations).map(v => JSON.parse(v));
  return { result: unique.length ? 'violation' : 'pass', violations: unique };
}

module.exports = { check, violationSet };
