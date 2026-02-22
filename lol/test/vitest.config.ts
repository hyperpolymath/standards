// SPDX-License-Identifier: MIT AND LicenseRef-Palimpsest-0.8
// SPDX-FileCopyrightText: 2024-2025 Ehsaneddin Asgari and Contributors

import { defineConfig } from 'vitest/config'

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    include: ['**/*_test.res.mjs', '**/*.test.ts'],
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      exclude: [
        'node_modules/',
        'test/',
        '**/*.d.ts',
      ],
    },
    testTimeout: 10000,
  },
})
