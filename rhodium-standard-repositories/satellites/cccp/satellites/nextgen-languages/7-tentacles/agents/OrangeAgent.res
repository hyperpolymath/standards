// OrangeAgent.res - The Concurrency Engine
// Teaches: Async/await, scheduling, event loops, concurrency

open Types

let names: agentNames = {
  cuttle: "Juggler Orange",
  squidlet: "Event Orange",
  duet: "Concurrency Agent Orange",
  octopus: "Concurrency Orchestrator Orange",
}

let personality: personality = {
  voice: "Calm and rhythmic, always counting beats and keeping things in sync",
  catchphrase: "Keep all the balls in the air!",
  encouragement: [
    "Great timing!",
    "You're keeping everything in sync!",
    "Perfect rhythm!",
    "You're juggling like a pro!",
  ],
  corrections: [
    "Oops! One ball dropped. Let's try again!",
    "The timing was a bit off there...",
    "Remember: each ball needs its moment!",
    "Let's slow down and find the rhythm.",
  ],
  celebrations: [
    "Amazing coordination!",
    "You kept everything spinning perfectly!",
    "Master juggler achievement unlocked!",
    "Not a single drop! Incredible!",
  ],
}

let teaches = [
  "Coordination and timing",
  "Sequencing multiple tasks",
  "Event-driven thinking",
  "Queue management",
  "Async/await patterns",
  "Promise chains",
  "Race condition awareness",
  "Scheduler design",
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
  | Cuttle => "Timing and coordination basics"
  | Squidlet => "Event systems and queues"
  | Duet => "Async/await and promises"
  | Octopus => "Concurrent system architecture"
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
  color: Orange,
  names,
  compilerRole: "Concurrency Engine - Manages parallel execution and scheduling",
  teaches,
  personality,
  lessons: [], // Populated from curriculum files
}

// Reveal text shown when transitioning stages
let revealText = (fromStage: stage, toStage: stage): option<string> => {
  switch (fromStage, toStage) {
  | (Cuttle, Squidlet) =>
    Some("All that juggling? Orange wasn't just teaching you to catch balls - Orange was teaching you to handle EVENTS. Each ball was like a task waiting for its turn!")
  | (Squidlet, Duet) =>
    Some("Orange has been teaching you CONCURRENCY! When you juggled multiple balls, you were learning how computers handle many tasks at once without dropping any.")
  | (Duet, Octopus) =>
    Some("You understand concurrent systems now! You know how Orange schedules which task runs when, preventing race conditions and keeping everything in harmony.")
  | _ => None
  }
}
