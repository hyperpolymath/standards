# TypeScript/JavaScript → ReScript Conversion Required

This repository is **entirely TS/JS** and needs full conversion to ReScript.

## Conversion Steps
1. Install ReScript: Add to deno.json or use npm: specifier
2. Create `rescript.json` configuration
3. For EACH file:
   - Create `.res` equivalent
   - Migrate types (ReScript has excellent type inference)
   - Update module imports
   - Test with `rescript build`
4. Delete all `.ts`/`.tsx`/`.js`/`.jsx` files
5. Remove TypeScript dependencies

## Policy
- No NEW TypeScript/JavaScript allowed (CI enforced)
- Existing code must be migrated to ReScript
- Generated `.res.js` files are the output

## Resources
- https://rescript-lang.org/docs/manual/latest/introduction
- https://rescript-lang.org/docs/manual/latest/migrate-from-typescript
