# Development Environment

I develop using emacs (using eglot for lsp support).

GitHub username: benbellick

# Code Style

## General Principles

- Prefer simplicity over cleverness
- Self-documenting code through clear naming is better than comments
- Avoid unnecessary abstractions

## Comments

Comments which serve as docstrings are excellent. On the other hand, comments on every line of code is an absolute no. Comments in code are only acceptable if the code is in some way confusing. If the code is confusing enough to warrant comments, it probably is poorly written and either better names should be used, or better logic generally.

## Naming Conventions

- Use descriptive names that reveal intent
- Avoid abbreviations unless they're widely understood in the domain
- Boolean variables should read like predicates (isValid, hasPermission, canSubmit)

## Code Organization

- Keep functions focused on a single responsibility
- Prefer small, composable functions over large monolithic ones
- Group related functionality together

# Testing & Quality

## Testing Philosophy

- Write tests for critical business logic and edge cases
- Unit tests should be fast and isolated
- Integration tests for key user workflows
- Don't test trivial code or framework functionality

## Error Handling

- Handle errors at appropriate boundaries
- Provide clear, actionable error messages
- Validate at system boundaries (user input, external APIs)
- Trust internal code and framework guarantees

# Communication Style

## When Working Together

- Be concise and direct in explanations
- Show code examples rather than lengthy descriptions
- Ask questions when requirements are ambiguous
- Don't over-explain obvious changes

## Planning and Execution

- Use todo lists for multi-step tasks to track progress
- Read existing code before proposing changes
- Make only the changes requested, avoid "improvements" beyond scope
- No need for time estimates in plans

## File Operations

- Strongly prefer using Write/Edit tools directly over shell heredocs (`cat > file << 'EOF'`)
- Only use heredocs when there's a good reason (e.g., complex shell scripts that need inline execution)

# Version Control

## Commits

- Do not commit code unless explicitly asked
- Wait for my instruction before creating commits or pull requests
- Use conventional commit format (e.g., `feat:`, `fix:`, `refactor:`, `docs:`, `chore:`)
- Include scope when relevant (e.g., `feat(auth):`, `fix(api):`)

## Pull Requests

- PR descriptions should be concise, like a good commit message
- Include a "Closes #ISSUE" reference when applicable (GitHub or Jira)
- Avoid AI-style walls of text with excessive sections and bullet points
