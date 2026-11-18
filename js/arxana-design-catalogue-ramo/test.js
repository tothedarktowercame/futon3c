'use strict';
const { test } = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const { check, violationSet } = require('./index');

const file = path.resolve(__dirname, '../../test/resources/arxana/design-catalogue-checks.json');
const shared = JSON.parse(fs.readFileSync(file, 'utf8'));
assert.equal(shared.format, 'design-catalogue-ground-fixtures-v1');
assert.ok(shared.cases.length > 0, 'must actually check the shared corpus');
for (const fixture of shared.cases) {
  test(`core.logic / ramo: ${fixture.id}`, () => {
    const actual = check(fixture);
    assert.equal(actual.result, fixture.expected.result);
    assert.deepEqual(violationSet(actual.violations), violationSet(fixture.expected.violations));
  });
}

test('ramo-only plant: an undocumented backend feature is a violation', () => {
  const actual = check({ check: 'coverage', facts: { documented: ['known'], citations: ['ramo-only-missing'] } });
  assert.deepEqual(actual, { result: 'violation', violations: ['ramo-only-missing'] });
  assert.throws(() => assert.equal(actual.result, 'pass'), assert.AssertionError);
});

test('unknown check is an error, never a passing result', () => {
  assert.throws(() => check({ check: 'not-a-check', facts: {} }), /Unknown catalogue check/);
});
