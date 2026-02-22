// Types.res - Shared types for the Seven Tentacles agent system

// Age stages in the cephalopod journey
type stage =
  | Cuttle      // Ages 8-12
  | Squidlet    // Ages 13-14
  | Duet        // Age 15
  | Octopus     // Ages 16+

// Agent colors matching compiler components
type agentColor =
  | Red     // Parser
  | Orange  // Concurrency
  | Yellow  // Type System
  | Green   // AST Architect
  | Blue    // Auditor
  | Indigo  // Metaprogrammer
  | Violet  // Governance

// Lesson difficulty levels
type difficulty =
  | Introductory
  | Beginner
  | Intermediate
  | Advanced
  | Expert

// A lesson in the curriculum
type lesson = {
  id: string,
  title: string,
  agent: agentColor,
  stage: stage,
  difficulty: difficulty,
  description: string,
  objectives: array<string>,
  activities: array<activity>,
  hiddenConcept: string,     // What they're really learning
  revealedConcept: option<string>, // What it becomes at reveal
}

// An activity within a lesson
and activity = {
  activityId: string,
  activityType: activityType,
  instructions: string,
  hints: array<string>,
}

// Types of activities
and activityType =
  | Game(gameConfig)
  | Puzzle(puzzleConfig)
  | Creative(creativeConfig)
  | Challenge(challengeConfig)

// Game configuration
and gameConfig = {
  gameName: string,
  rules: array<string>,
  winCondition: string,
}

// Puzzle configuration
and puzzleConfig = {
  puzzleType: string,
  pieces: int,
  solution: string,
}

// Creative activity configuration
and creativeConfig = {
  medium: string,
  prompt: string,
}

// Challenge configuration
and challengeConfig = {
  challengeType: string,
  timeLimit: option<int>,
  scoring: string,
}

// Agent personality traits
type personality = {
  voice: string,         // How they speak
  catchphrase: string,   // Their signature line
  encouragement: array<string>,
  corrections: array<string>,
  celebrations: array<string>,
}

// Agent name at different stages
type agentNames = {
  cuttle: string,
  squidlet: string,
  duet: string,
  octopus: string,
}

// Complete agent definition
type agent = {
  color: agentColor,
  names: agentNames,
  compilerRole: string,
  teaches: array<string>,
  personality: personality,
  lessons: array<lesson>,
}

// Progress tracking
type learnerProgress = {
visitorId: string,
  currentStage: stage,
  completedLessons: array<string>,
  currentLesson: option<string>,
  favoriteAgent: option<agentColor>,
  startDate: float,
  lastActive: float,
}

// Helper functions
let stageToAge = (s: stage): (int, int) => {
  switch s {
  | Cuttle => (8, 12)
  | Squidlet => (13, 14)
  | Duet => (15, 15)
  | Octopus => (16, 99)
  }
}

let colorToEmoji = (c: agentColor): string => {
  switch c {
  | Red => "🔴"
  | Orange => "🟠"
  | Yellow => "🟡"
  | Green => "🟢"
  | Blue => "🔵"
  | Indigo => "🟣"
  | Violet => "🟤"
  }
}

let colorToHex = (c: agentColor): string => {
  switch c {
  | Red => "#E74C3C"
  | Orange => "#E67E22"
  | Yellow => "#F1C40F"
  | Green => "#2ECC71"
  | Blue => "#3498DB"
  | Indigo => "#9B59B6"
  | Violet => "#8E44AD"
  }
}
