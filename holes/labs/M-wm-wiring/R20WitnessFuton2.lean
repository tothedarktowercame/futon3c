import DarkTower.WarMachine.R20Witness

namespace DarkTower.WarMachine.R20Witness.Generated
open DarkTower.WarMachine.R20Witness
open DarkTower.WarMachine.R20Graph

def futon2Witness : Witness :=
  { sha := "270bebc27965b31f1d33012e68b4d1f512eca3f9"
    edges := [
    ⟨.selectionToPrecisionHabit, [{ writer := "futon2/src/futon2/aif/policy.clj#selection-candidate", field := ":habit", record := ":selection-candidate", fieldType := "quantity/policy-prior", reader := "futon2/src/futon2/aif/policy_precision_carry.clj#family", confirmed := false, sha := "270bebc27965b31f1d33012e68b4d1f512eca3f9" }, { writer := "futon2/src/futon2/aif/policy_precision_carry.clj#family", field := ":candidates", record := ":precision-family", fieldType := "receipt/frozen-policy-family", reader := "futon2/src/futon2/aif/policy_precision_carry.clj#advance", confirmed := false, sha := "270bebc27965b31f1d33012e68b4d1f512eca3f9" }]⟩,
    ⟨.selectionToPrecisionG, [{ writer := "futon2/src/futon2/aif/policy.clj#selection-candidate", field := ":g", record := ":selection-candidate", fieldType := "quantity/expected-free-energy", reader := "futon2/src/futon2/aif/policy_precision_carry.clj#family", confirmed := false, sha := "270bebc27965b31f1d33012e68b4d1f512eca3f9" }, { writer := "futon2/src/futon2/aif/policy_precision_carry.clj#family", field := ":candidates", record := ":precision-family", fieldType := "receipt/frozen-policy-family", reader := "futon2/src/futon2/aif/policy_precision_carry.clj#advance", confirmed := false, sha := "270bebc27965b31f1d33012e68b4d1f512eca3f9" }]⟩,
    ⟨.inferenceToPrecisionF, [{ writer := "futon2/src/futon2/aif/policy_precision_carry.clj#retrospective-f", field := ":f", record := ":precision-retrospective", fieldType := "quantity/free-energy", reader := "futon2/src/futon2/aif/policy_precision_carry.clj#advance", confirmed := true, sha := "270bebc27965b31f1d33012e68b4d1f512eca3f9" }]⟩] }

#eval verdicts futon2Witness

end DarkTower.WarMachine.R20Witness.Generated
