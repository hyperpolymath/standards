// RevealSystem.res - The Progressive Reveal System
// Manages age-based transitions and concept revelations

open Types

// Import all agents
module Red = RedAgent
module Orange = OrangeAgent
module Yellow = YellowAgent
module Green = GreenAgent
module Blue = BlueAgent
module Indigo = IndigoAgent
module Violet = VioletAgent

// Determine the stage based on age
let stageFromAge = (age: int): stage => {
  if age < 8 {
    Cuttle // Pre-program, but use Cuttle as default
  } else if age <= 12 {
    Cuttle
  } else if age <= 14 {
    Squidlet
  } else if age == 15 {
    Duet
  } else {
    Octopus
  }
}

// Get the mascot name for a stage
let mascotName = (s: stage): string => {
  switch s {
  | Cuttle => "Cuttle the Cuttlefish"
  | Squidlet => "Squidlet the Growing Squid"
  | Duet => "The Dancing Duet"
  | Octopus => "Octavia the Octopus"
  }
}

// Get the mascot description for a stage
let mascotDescription = (s: stage): string => {
  switch s {
  | Cuttle => "A curious baby cuttlefish exploring the ocean, learning one thing at a time"
  | Squidlet => "An adolescent squid growing bigger, starting to see how things connect"
  | Duet => "Two squid dancing together, learning to work as a team"
  | Octopus => "A wise octopus with all eight tentacles working in harmony"
  }
}

// Get the language for a stage
let languageName = (s: stage): string => {
  switch s {
  | Cuttle => "Me Language"
  | Squidlet => "Solo Language"
  | Duet => "Duet Language"
  | Octopus => "Ensemble Language"
  }
}

// Get the language description for a stage
let languageDescription = (s: stage): string => {
  switch s {
  | Cuttle => "Visual blocks that snap together - no typing needed!"
  | Squidlet => "Text-based code with helpful types that keep things organized"
  | Duet => "Protocols for two agents to work together"
  | Octopus => "Full orchestration of all seven agents building compilers"
  }
}

// Get an agent's name for the current stage
let getAgentName = (color: agentColor, stage: stage): string => {
  switch color {
  | Red => Red.getName(stage)
  | Orange => Orange.getName(stage)
  | Yellow => Yellow.getName(stage)
  | Green => Green.getName(stage)
  | Blue => Blue.getName(stage)
  | Indigo => Indigo.getName(stage)
  | Violet => Violet.getName(stage)
  }
}

// Get what an agent secretly teaches at the current stage
let getHiddenConcept = (color: agentColor, stage: stage): string => {
  switch color {
  | Red => Red.getHiddenConcept(stage)
  | Orange => Orange.getHiddenConcept(stage)
  | Yellow => Yellow.getHiddenConcept(stage)
  | Green => Green.getHiddenConcept(stage)
  | Blue => Blue.getHiddenConcept(stage)
  | Indigo => Indigo.getHiddenConcept(stage)
  | Violet => Violet.getHiddenConcept(stage)
  }
}

// Get the reveal text when transitioning stages
let getRevealText = (color: agentColor, fromStage: stage, toStage: stage): option<string> => {
  switch color {
  | Red => Red.revealText(fromStage, toStage)
  | Orange => Orange.revealText(fromStage, toStage)
  | Yellow => Yellow.revealText(fromStage, toStage)
  | Green => Green.revealText(fromStage, toStage)
  | Blue => Blue.revealText(fromStage, toStage)
  | Indigo => Indigo.revealText(fromStage, toStage)
  | Violet => Violet.revealText(fromStage, toStage)
  }
}

// Structure for a complete stage transition reveal
type stageReveal = {
  fromStage: stage,
  toStage: stage,
  mascotChange: string,
  languageChange: string,
  agentReveals: array<(agentColor, string)>,
}

// Generate the complete reveal when transitioning stages
let generateStageReveal = (fromStage: stage, toStage: stage): stageReveal => {
  let colors = [Red, Orange, Yellow, Green, Blue, Indigo, Violet]

  let agentReveals = colors->Array.filterMap(color => {
    getRevealText(color, fromStage, toStage)->Option.map(text => (color, text))
  })

  {
    fromStage,
    toStage,
    mascotChange: `${mascotName(fromStage)} is growing up into ${mascotName(toStage)}!`,
    languageChange: `You're ready for ${languageName(toStage)}: ${languageDescription(toStage)}`,
    agentReveals,
  }
}

// The "Big Reveal" at age 16 - when everything comes together
let theBigReveal = (): string => {
  `
=== THE BIG REVEAL ===

You've been on an incredible journey!

Remember Speedy Red? Those racing games?
Red was teaching you PARSING - how compilers read code!

Remember Juggler Orange? All that coordination?
Orange was teaching you CONCURRENCY - how computers handle many tasks!

Remember Safety Yellow? The sorting games?
Yellow was teaching you TYPE SYSTEMS - how to prevent bugs!

Remember Builder Green? Those construction projects?
Green was teaching you AST ARCHITECTURE - how code is structured!

Remember Detective Blue? Those mystery puzzles?
Blue was teaching you VERIFICATION - how to prove code is correct!

Remember Magic Indigo? Those spell-casting games?
Indigo was teaching you METAPROGRAMMING - code that writes code!

Remember Teacher Violet? Those fair-play rules?
Violet was teaching you LANGUAGE DESIGN - how programming languages are made!

For 8 YEARS, you've been learning COMPILER ARCHITECTURE.

And now? You can build your own programming language.

Welcome to the Octopus stage. All eight tentacles are yours.

=== BUILD SOMETHING AMAZING ===
`
}

// Calculate progress through the curriculum
type curriculumProgress = {
  currentStage: stage,
  lessonsCompleted: int,
  lessonsTotal: int,
  percentComplete: float,
  nextMilestone: string,
}

let calculateProgress = (
  completedLessons: array<string>,
  currentStage: stage
): curriculumProgress => {
  let lessonsCompleted = Array.length(completedLessons)

  let (lessonsTotal, nextMilestone) = switch currentStage {
  | Cuttle => (140, "Complete all Cuttle lessons to become a Squidlet!")
  | Squidlet => (350, "Complete all Squidlet lessons to enter the Duet stage!")
  | Duet => (420, "Complete all Duet lessons to become an Octopus!")
  | Octopus => (500, "You're at the top! Keep building amazing things!")
  }

  let percentComplete = Int.toFloat(lessonsCompleted) /. Int.toFloat(lessonsTotal) *. 100.0

  {
    currentStage,
    lessonsCompleted,
    lessonsTotal,
    percentComplete,
    nextMilestone,
  }
}

// Check if a learner is ready to advance to the next stage
let canAdvance = (progress: curriculumProgress): bool => {
  switch progress.currentStage {
  | Cuttle => progress.lessonsCompleted >= 140
  | Squidlet => progress.lessonsCompleted >= 350
  | Duet => progress.lessonsCompleted >= 420
  | Octopus => false // Already at top
  }
}

// Get the next stage (if available)
let nextStage = (current: stage): option<stage> => {
  switch current {
  | Cuttle => Some(Squidlet)
  | Squidlet => Some(Duet)
  | Duet => Some(Octopus)
  | Octopus => None
  }
}
