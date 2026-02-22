// GreenAgent.res - The AST Architect
// Teaches: Abstract syntax trees, code representation, manipulation

open Types

let names: agentNames = {
  cuttle: "Builder Green",
  squidlet: "Maker Green",
  duet: "Structure Agent Green",
  octopus: "Code Architecture Specialist Green",
}

let personality: personality = {
  voice: "Creative and constructive, always excited about building new things",
  catchphrase: "Let's build something amazing!",
  encouragement: [
    "Great foundation!",
    "That structure is solid!",
    "You're building beautifully!",
    "Perfect piece placement!",
  ],
  corrections: [
    "Hmm, that piece doesn't connect there...",
    "Let's check the blueprint again.",
    "The structure needs a stronger base.",
    "Almost! But buildings need foundations first.",
  ],
  celebrations: [
    "What an amazing creation!",
    "Architectural masterpiece!",
    "You built something incredible!",
    "That structure will stand forever!",
  ],
}

let teaches = [
  "Composition and construction",
  "Hierarchical thinking",
  "Tree structures",
  "Data representation",
  "Abstract syntax trees",
  "Intermediate representations",
  "Code generation",
  "Optimization passes",
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
  | Cuttle => "Composition and hierarchy"
  | Squidlet => "Tree structures and data representation"
  | Duet => "AST construction and manipulation"
  | Octopus => "Compiler IR and code generation"
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
  color: Green,
  names,
  compilerRole: "AST Architect - Builds and transforms code representations",
  teaches,
  personality,
  lessons: [], // Populated from curriculum files
}

// Reveal text shown when transitioning stages
let revealText = (fromStage: stage, toStage: stage): option<string> => {
  switch (fromStage, toStage) {
  | (Cuttle, Squidlet) =>
    Some("All those building blocks? Green was teaching you about STRUCTURE! Every tower you built was like a tree of code, with branches and leaves.")
  | (Squidlet, Duet) =>
    Some("Green has been teaching you about ABSTRACT SYNTAX TREES! When you assembled pieces into complex structures, you were learning how compilers represent code internally.")
  | (Duet, Octopus) =>
    Some("You understand code architecture now! You know how Green transforms human-readable code into tree structures that computers can execute.")
  | _ => None
  }
}
