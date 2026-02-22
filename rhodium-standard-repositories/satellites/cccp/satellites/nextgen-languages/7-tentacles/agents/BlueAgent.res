// BlueAgent.res - The Auditor
// Teaches: Verification, auditing, tracing, debugging, proof systems

open Types

let names: agentNames = {
  cuttle: "Detective Blue",
  squidlet: "Tracker Blue",
  duet: "Verification Agent Blue",
  octopus: "Verification Oracle Blue",
}

let personality: personality = {
  voice: "Curious and analytical, always asking questions and looking for clues",
  catchphrase: "The evidence never lies!",
  encouragement: [
    "Excellent deduction!",
    "You found a crucial clue!",
    "Your logic is impeccable!",
    "The mystery is unraveling!",
  ],
  corrections: [
    "Hmm, let's re-examine the evidence...",
    "That clue might mean something else.",
    "Good theory, but let's verify it.",
    "The facts don't quite add up yet.",
  ],
  celebrations: [
    "Case solved! Brilliant detective work!",
    "You've cracked the code!",
    "Irrefutable proof! Amazing!",
    "The mystery is completely solved!",
  ],
}

let teaches = [
  "Logical deduction",
  "Evidence gathering",
  "Proof construction",
  "Debugging techniques",
  "Execution tracing",
  "Hoare logic",
  "Formal verification",
  "Theorem proving",
]

// Get the agent's name for a given stage
let getName = (stage: stage): string => {
  switch stage {
  | Cuttle => names.cuttle
  | Squidlet => names.squidlet
  | Duet => names.duet
  | Octopus => names.octopus
  }
}

// Get what the agent is "secretly" teaching at each stage
let getHiddenConcept = (stage: stage): string => {
  switch stage {
  | Cuttle => "Logical deduction and proof"
  | Squidlet => "Logging and execution tracing"
  | Duet => "Formal verification and Hoare logic"
  | Octopus => "Theorem proving and certified systems"
  }
}

// Get a random encouragement message
let encourage = (): string => {
  let idx = Js.Math.random_int(0, Array.length(personality.encouragement))
  personality.encouragement[idx]->Option.getOr(personality.catchphrase)
}

// Get a random correction message
let correct = (): string => {
  let idx = Js.Math.random_int(0, Array.length(personality.corrections))
  personality.corrections[idx]->Option.getOr("Let's try again!")
}

// Get a random celebration message
let celebrate = (): string => {
  let idx = Js.Math.random_int(0, Array.length(personality.celebrations))
  personality.celebrations[idx]->Option.getOr(personality.catchphrase)
}

// Create the complete agent definition
let agent: agent = {
  color: Blue,
  names,
  compilerRole: "Auditor - Verifies correctness and provides formal proofs",
  teaches,
  personality,
  lessons: [], // Populated from curriculum files
}

// Reveal text shown when transitioning stages
let revealText = (fromStage: stage, toStage: stage): option<string> => {
  switch (fromStage, toStage) {
  | (Cuttle, Squidlet) =>
    Some("All those mysteries you solved? Blue was teaching you about VERIFICATION! Every clue you found was like evidence that proves code is correct.")
  | (Squidlet, Duet) =>
    Some("Blue has been teaching you FORMAL VERIFICATION! When you proved who did it, you were learning how mathematicians prove that code can never fail.")
  | (Duet, Octopus) =>
    Some("You understand verification systems now! You know how Blue can mathematically PROVE that code is correct, not just test it.")
  | _ => None
  }
}
