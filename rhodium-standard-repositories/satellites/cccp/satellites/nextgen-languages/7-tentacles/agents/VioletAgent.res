// VioletAgent.res - The Governance System
// Teaches: Language design, policy enforcement, access control, ethics

open Types

let names: agentNames = {
  cuttle: "Teacher Violet",
  squidlet: "Judge Violet",
  duet: "Language Designer Violet",
  octopus: "Governance Architect Violet",
}

let personality: personality = {
  voice: "Wise and fair, always explaining the reasons behind rules",
  catchphrase: "Fair rules make better games for everyone!",
  encouragement: [
    "You understand fairness!",
    "That rule protects everyone!",
    "Wise decision!",
    "You're thinking about others!",
  ],
  corrections: [
    "Hmm, that rule might not be fair to everyone...",
    "Let's think about who this affects.",
    "Good intention, but consider the consequences.",
    "Rules need to work for everyone.",
  ],
  celebrations: [
    "A fair and just system!",
    "Everyone can play happily now!",
    "You've created something beautiful and fair!",
    "True wisdom in governance!",
  ],
}

let teaches = [
  "Fairness and rules",
  "Cooperation and ethics",
  "System design",
  "Policy creation",
  "Constraint languages",
  "Domain-specific language design",
  "Access control",
  "Language philosophy",
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
  | Cuttle => "Fairness and rule-making"
  | Squidlet => "Constraints and policy enforcement"
  | Duet => "Language design principles"
  | Octopus => "Ethical system architecture"
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
  color: Violet,
  names,
  compilerRole: "Governance System - Designs language rules and enforces ethical constraints",
  teaches,
  personality,
  lessons: [], // Populated from curriculum files
}

// Reveal text shown when transitioning stages
let revealText = (fromStage: stage, toStage: stage): option<string> => {
  switch (fromStage, toStage) {
  | (Cuttle, Squidlet) =>
    Some("All those fair games? Violet was teaching you about GOVERNANCE! Every rule you created was like designing a programming language.")
  | (Squidlet, Duet) =>
    Some("Violet has been teaching you LANGUAGE DESIGN! When you made rules for your games, you were learning how programming languages are created.")
  | (Duet, Octopus) =>
    Some("You understand language governance now! You know how Violet designs rules that make systems fair, safe, and accessible for everyone.")
  | _ => None
  }
}
