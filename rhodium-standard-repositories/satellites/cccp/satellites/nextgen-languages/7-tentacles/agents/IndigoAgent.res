// IndigoAgent.res - The Compile-Time Metaprogrammer
// Teaches: Metaprogramming, compile-time evaluation, macros, reflection

open Types

let names: agentNames = {
  cuttle: "Magic Indigo",
  squidlet: "Spell Indigo",
  duet: "Metaprogramming Wizard Indigo",
  octopus: "Compile-Time Execution Master Indigo",
}

let personality: personality = {
  voice: "Mysterious and whimsical, speaking in riddles that reveal deep truths",
  catchphrase: "The real magic happens before the show begins!",
  encouragement: [
    "Your spell is taking shape!",
    "The magic flows through you!",
    "What a powerful incantation!",
    "You're learning the ancient ways!",
  ],
  corrections: [
    "The spell fizzled... let's adjust the formula.",
    "Magic requires precise ingredients.",
    "Almost! But the timing was off.",
    "That spell needs more preparation.",
  ],
  celebrations: [
    "MAGNIFICENT MAGIC!",
    "You've mastered the arcane arts!",
    "A spell for the ages!",
    "True wizardry! Extraordinary!",
  ],
}

let teaches = [
  "Transformation and pattern rules",
  "Abstraction and shortcuts",
  "Template systems",
  "Macro programming",
  "Compile-time computation",
  "Staged computation",
  "Partial evaluation",
  "Code generation",
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
  | Cuttle => "Transformation and rule-based patterns"
  | Squidlet => "Macros and templating"
  | Duet => "Compile-time evaluation and staging"
  | Octopus => "Partial evaluation and supercompilation"
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
  color: Indigo,
  names,
  compilerRole: "Metaprogrammer - Executes code at compile time to generate optimized code",
  teaches,
  personality,
  lessons: [], // Populated from curriculum files
}

// Reveal text shown when transitioning stages
let revealText = (fromStage: stage, toStage: stage): option<string> => {
  switch (fromStage, toStage) {
  | (Cuttle, Squidlet) =>
    Some("All that magic? Indigo was teaching you about TRANSFORMATION! Every spell you cast was like a program that writes other programs.")
  | (Squidlet, Duet) =>
    Some("Indigo has been teaching you METAPROGRAMMING! When you cast spells that created new spells, you were learning how code can generate code.")
  | (Duet, Octopus) =>
    Some("You understand compile-time execution now! You know how Indigo runs code BEFORE the program runs, creating specialized, optimized programs.")
  | _ => None
  }
}
