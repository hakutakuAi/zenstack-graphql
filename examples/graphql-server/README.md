# GraphQL Server Example

A simple GraphQL server example using ZenStack GraphQL plugin and GraphQL Yoga.

## Setup

1. Install dependencies:
   ```bash
   bun install
   ```

2. Generate GraphQL schema from ZModel:
   ```bash
   bun run setup
   ```

3. Start the server:
   ```bash
   bun run start
   ```

4. Now you can open GraphQL Playground on [http://localhost:4000/graphql](http://localhost:4000/graphql)

## Run tests

1. Make sure you have installed depedencies and runned the setup command
2. Run tests:
   ```bash
   bun test
   ```