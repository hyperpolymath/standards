// RedAgent.res - The Parser
// Teaches: Lexical analysis → Parsing → Syntax trees

open Types

let names: agentNames = {
  cuttle: "Speedy Red",
  squidlet: "Fast Finder Red",
  duet: "Performance Agent Red",
  octopus: "Performance Agent Red",
}

let personality: personality = {
  voice: "Energetic and fast-talking, always excited about speed and efficiency",
  catchphrase: "Let's zoom through this!",
  encouragement: [
    "You're getting faster!",
    "That pattern was perfect!",
    "Speedy work!",
    "You're thinking like a racer now!",
  ],
  corrections: [
    "Hmm, let's try a different path!",
    "Almost! What if we went faster here?",
    "Good try! Speed isn't just about going fast...",
    "Let's look at this pattern again!",
  ],
  celebrations: [
    "ZOOM! You did it!",
    "New record! Amazing!",
    "That was lightning fast!",
    "You've mastered this track!",
  ],
}

let teaches = [
  "Pattern recognition",
  "Algorithmic thinking",
  "Efficiency and optimization",
  "Lexical analysis",
  "Recursive descent parsing",
  "Grammar and syntax rules",
  "Tokenization",
  "Abstract syntax tree construction",
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
  | Cuttle => "Pattern recognition and rule-following"
  | Squidlet => "Algorithmic complexity and optimization"
  | Duet => "Lexical analysis and parsing"
  | Octopus => "Complete parser implementation"
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
  color: Red,
  names,
  compilerRole: "Parser - Transforms source code into structured syntax trees",
  teaches,
  personality,
  lessons: [], // Populated from curriculum files
}

// Reveal text shown when transitioning stages
let revealText = (fromStage: stage, toStage: stage): option<string> => {
  switch (fromStage, toStage) {
  | (Cuttle, Squidlet) =>
    Some("Remember all those racing games? Red wasn't just teaching you to go fast - Red was teaching you to find PATTERNS. Every race track was like a sentence, and you learned to read them!")
  | (Squidlet, Duet) =>
    Some("Red has been teaching you PARSING all along! When you found the fastest path through obstacles, you were learning how compilers break down code into pieces they can understand.")
  | (Duet, Octopus) =>
    Some("You've mastered what Red teaches: lexical analysis and parsing. You can now build the first stages of a real compiler!")
  | _ => None
  }
}
