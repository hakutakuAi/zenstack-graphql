# ZenStack GraphQL Plugin

<div align="center">

[![CI/CD Pipeline](https://github.com/ViniciosLugli/zenstack-graphql/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/ViniciosLugli/zenstack-graphql/actions/workflows/ci.yml)
[![License](https://img.shields.io/badge/License-GPL--3.0-blue.svg)](LICENSE)
[![npm version](https://badge.fury.io/js/%40hakutakuai%2Fzenstack-graphql.svg)](https://www.npmjs.com/package/@hakutakuai/zenstack-graphql)
[![npm downloads](https://img.shields.io/npm/dm/@hakutakuai/zenstack-graphql.svg)](https://www.npmjs.com/package/@hakutakuai/zenstack-graphql)
[![TypeScript](https://img.shields.io/badge/TypeScript-007ACC?style=flat&logo=typescript&logoColor=white)](https://www.typescriptlang.org/)
[![Bun](https://img.shields.io/badge/Bun-%23000000.svg?style=flat&logo=bun&logoColor=white)](https://bun.sh)

</div>

---

This plugin generates a complete GraphQL schema with relations, filtering, sorting, and pagination. Saving you hours of boilerplate code to match your database models with you graphql server.

## Features

- **Zero Boilerplate**: Go from data model to full GraphQL API schema using the [Zenstack](https://zenstack.dev/)
- **Dual Output Formats**: Generate either GraphQL SDL schemas or TypeGraphQL TypeScript classes
- **TypeGraphQL Support**: Generate type-safe TypeScript classes with decorators for TypeGraphQL-based servers
- **Relationship Handling**: All your model relations become GraphQL connections automatically
- **Modern API Features**: Built-in filtering, sorting and Relay-compatible pagination
- **Customization That Makes Sense**: Rename fields, customize types, hide sensitive data, ignore generations...

## How to use

> TL;DR - See the [graphql-server example](./examples/graphql-server/) with full code example

Install with your favorite package manager:

```bash
# Using bun (Recommended)
bun add @hakutakuai/zenstack-graphql -D

# Using npm
npm install @hakutakuai/zenstack-graphql --save-dev

# Using yarn
yarn add @hakutakuai/zenstack-graphql -D

#...
```

Add to your schema.zmodel file:

> Just a base example

```zmodel
plugin graphql {
    provider = '@hakutakuai/zenstack-graphql'
    output = './schema.graphql'
    outputFormat = 'graphql'  // or 'type-graphql' for TypeScript classes
}
```

Generate your GraphQL schema:

```bash
zenstack generate
```

That's it! Your GraphQL schema is ready to use with any GraphQL server.

### TypeGraphQL Alternative

For TypeScript projects using TypeGraphQL, you can generate TypeScript classes instead:

```zmodel
plugin graphql {
    provider = '@hakutakuai/zenstack-graphql'
    output = './schema.ts'
    outputFormat = 'type-graphql'
}
```

This generates TypeScript classes with TypeGraphQL decorators instead of SDL.

## Customization Options

Keep your GraphQL schema clean to your needs:

```zmodel
plugin graphql {
    // Basic settings
    output = './schema.graphql'
    outputFormat = 'graphql'    // 'graphql' for SDL or 'type-graphql' for TypeScript

    // Use your preferred naming style
    fieldNaming = 'snake_case'  // Fields become like_this
    typeNaming = 'PascalCase'   // Types become LikeThis

    // Customize scalar type mappings
    scalarTypes = {
        "DateTime": 'Date',      // Use Date instead of DateTime
        "Json": 'JSONObject'     // Use JSONObject instead of JSON
    }

    // Turn features on/off as needed
    connectionTypes = true     // Add Relay pagination support
    generateFilters = true     // Add filtering capabilities
    generateSorts = true       // Add sorting functionality
}
```

### Fine-tune with Attributes

Control exactly what appears in your API:

```zmodel
model User {
    id          Int      @id
    email       String   @unique

    // Hide sensitive fields from the API
    password    String   @graphql.ignore

    // Rename fields for better API design
    emailAddress String  @graphql.name("email")

    // Add clear descriptions for your API docs
    fullName    String   @graphql.description("User's complete name")

    // Mark fields for filtering and sorting
    createdAt   DateTime @graphql.sortable @graphql.filterable

    // Model-level customization
    @@graphql.name("Member")                  // Rename the type
    @@graphql.description("Platform member")  // Add type description
    @@graphql.connection(pageSize: 20)        // Configure pagination
}
```

## Configuration Reference

Here's a complete reference of all available options:

| Option             | Type      | Default              | Description                                                           |
| ------------------ | --------- | -------------------- | --------------------------------------------------------------------- |
| `output`           | `string`  | `'./schema.graphql'` | Output path for the generated schema                                  |
| `outputFormat`     | `string`  | `'graphql'`          | Output format: `'graphql'` for SDL or `'type-graphql'` for TypeScript |
| `scalarTypes`      | `object`  | _See below_          | Custom mappings for scalar types                                      |
| `connectionTypes`  | `boolean` | `true`               | Generate Relay-compatible connection types                            |
| `generateEnums`    | `boolean` | `true`               | Generate GraphQL enum types                                           |
| `generateScalars`  | `boolean` | `true`               | Generate GraphQL scalar types                                         |
| `generateFilters`  | `boolean` | `true`               | Generate filter input types                                           |
| `generateSorts`    | `boolean` | `true`               | Generate sort input types                                             |
| `fieldNaming`      | `string`  | `'camelCase'`        | Field naming convention                                               |
| `typeNaming`       | `string`  | `'PascalCase'`       | Type naming convention                                                |
| `includeRelations` | `boolean` | `true`               | Include model relations in schema                                     |

Default scalar mappings:

```
{
  "DateTime": 'DateTime',
  "Json": 'JSON',
  "Decimal": 'Decimal',
  "Bytes": 'String'
}
```

> You can see a example on [custom-naming](examples/custom-naming)

## Helper Utilities

The plugin provides powerful helper utilities to make your resolver implementation easier and more efficient:

### Available Helpers

```typescript
import {
	buildConnection, // Create Relay-compatible connections
	buildFilter, // Process filter input arguments
	buildSort, // Process sort input arguments
	buildSelect, // Create Prisma select objects based on GraphQL fields
	QueryBuilder, // Comprehensive query builder
} from '@hakutakuai/zenstack-graphql/helpers'
```

### Select Definitions Pattern

For efficient and type-safe resolvers, you can define reusable select objects:

```typescript
// src/select-definitions.ts
import type { Prisma } from '@prisma/client'

// Base select objects for each model
export const USER_BASE_SELECT = {
	id: true,
	name: true,
	email: true,
} satisfies Prisma.UserSelect

// Select objects with relations
export const USER_WITH_POSTS_SELECT = {
	...USER_BASE_SELECT,
	posts: {
		select: POST_BASE_SELECT,
	},
} satisfies Prisma.UserSelect

// Type definitions for select return types
export type UserWithPosts = Prisma.UserGetPayload<{
	select: typeof USER_WITH_POSTS_SELECT
}>
```

### Complete Example Flow

Here's how to use the helpers in your resolver:

```typescript
import { Resolver, Query, Arg, Info } from 'type-graphql'
import { GraphQLResolveInfo } from 'graphql'
import { buildConnection, buildFilter, buildSort, buildSelect } from '@hakutakuai/zenstack-graphql/helpers'
import { USER_WITH_POSTS_SELECT } from '../select-definitions'

@Resolver()
export class UserResolver {
	@Query(() => UserConnection)
	async users(
		// Pagination arguments
		@Arg('first', () => Int, { nullable: true }) first: number | undefined,
		@Arg('after', () => String, { nullable: true }) after: string | undefined,
		@Arg('last', () => Int, { nullable: true }) last: number | undefined,
		@Arg('before', () => String, { nullable: true }) before: string | undefined,
		// Filter and sort arguments
		@Arg('filter', () => UserFilterInput, { nullable: true }) filter: UserFilterInput | undefined,
		@Arg('sort', () => UserSortInput, { nullable: true }) sort: UserSortInput | undefined,
		// GraphQL info for field selection
		@Info() info: GraphQLResolveInfo,
		@Ctx() { prisma }: Context,
	): Promise<UserConnection> {
		// Build Prisma arguments
		const where = buildFilter(filter)
		const orderBy = buildSort(sort)
		const select = buildSelect(USER_WITH_POSTS_SELECT, info)

		// Create connection config
		const config = buildConnection({
			first,
			after,
			last,
			before,
			where,
			orderBy,
			select,
		})

		// Execute query and build connection response
		const [items, totalCount] = await Promise.all([prisma.user.findMany(config.findMany), prisma.user.count(config.count)])

		return config.toConnection(items, totalCount) as UserConnection
	}
}
```

## How It Works

The plugin transforms your ZModel schema into a GraphQL schema through these steps:

1. **Parsing**: Reads your data models and extracts entities, relationships, and attributes
2. **Type Generation**: Creates GraphQL types that match your data structure
3. **Relation Processing**: Maps your model relationships to GraphQL connections
4. **Connection Generation**: Builds Relay-compatible pagination support
5. **Schema Assembly**: Puts everything together into a complete GraphQL schema
6. **Output**: Writes the final schema to your specified location
7. **Helper Generation**: Provides utility functions for working with the generated schema

> All this happens automatically when you run `zenstack generate`, you can see more about on [Zenstack Plugin System](https://zenstack.dev/docs/the-complete-guide/part2/)!

## Use case example: Blog Schema

Here's how easy it is to create a blog API:

### ZModel Input

First, define your data models:

```zmodel
datasource db {
  provider = "postgresql"
  url = env("DATABASE_URL")
}

generator client {
  provider = "prisma-client-js"
}

plugin graphql {
  provider = "@hakutakuai/zenstack-graphql"
  output = "./schema.graphql"
}

model User {
  id        Int     @id @default(autoincrement())
  email     String  @unique
  name      String?
  posts     Post[]
}

model Post {
  id        Int     @id @default(autoincrement())
  title     String
  content   String?
  published Boolean @default(false)
  author    User    @relation(fields: [authorId], references: [id])
  authorId  Int
}
```

### Generated GraphQL Schema

Running `zenstack generate` automatically produces this GraphQL schema:

```graphql
"""
An object with a unique identifier
"""
interface Node {
	"""
	The unique identifier for this object
	"""
	id: ID!
}

"""
A date-time string at UTC, such as 2007-12-03T10:15:30Z
"""
scalar DateTime

"""
The `JSON` scalar type represents JSON values as specified by ECMA-404
"""
scalar JSON

"""
An arbitrary-precision Decimal type
"""
scalar Decimal

# Base model types
type User {
	id: Int!
	email: String!
	name: String
	posts: [Post!]!
}

type Post {
	id: Int!
	title: String!
	content: String
	published: Boolean!
	author: User!
	authorId: Int!
}

# Pagination support (Relay-compatible)
type PageInfo {
	hasNextPage: Boolean!
	hasPreviousPage: Boolean!
	startCursor: String
	endCursor: String
}

# Connection types for related lists
type UserConnection {
	pageInfo: PageInfo!
	edges: [UserEdge!]!
	totalCount: Int!
}

type UserEdge {
	node: User!
	cursor: String!
}

type PostConnection {
	pageInfo: PageInfo!
	edges: [PostEdge!]!
	totalCount: Int!
}

type PostEdge {
	node: Post!
	cursor: String!
}

# Filtering and sorting inputs
input StringFilterInput {
	equals: String
	not: String
	in: [String!]
	notIn: [String!]
	contains: String
	startsWith: String
	endsWith: String
}

input BooleanFilterInput {
	equals: Boolean
	not: Boolean
}

# ... and more generated types for complete API functionality
```

This simple schema definition gives you a GraphQL API with:

- **Custom Scalars**: DateTime, JSON, and Decimal scalar types
- **Base Types**: User and Post types with their relationships
- **Relay Connections**: Connection types for efficient pagination
- **Filtering**: Filter inputs for complex data queries
- **Sorting**: Sort inputs for ordering results
- **Standard Interfaces**: Node interface for unified type handling

No need to write connection types, or filtering logic, everything is automatically generated from your data model.

## TypeGraphQL Example

For TypeScript projects using TypeGraphQL, the plugin can generate TypeScript classes with decorators:

### ZModel Input

```zmodel
plugin graphql {
  provider = "@hakutakuai/zenstack-graphql"
  output = "./schema.ts"
  outputFormat = "type-graphql"
}

model User {
  id        String   @id @default(cuid())
  name      String
  email     String   @unique
  bio       String?
  posts     Post[]
}

model Post {
  id        String   @id @default(cuid())
  title     String
  content   String
  published Boolean  @default(false)
  author    User     @relation(fields: [authorId], references: [id])
  authorId  String
}
```

### Generated TypeScript Classes

```typescript
import { ObjectType, Field, registerEnumType } from 'type-graphql'
import { GraphQLJSON } from 'graphql-scalars'
import 'reflect-metadata'

@ObjectType()
export class User {
	@Field(() => String)
	id!: string

	@Field(() => String)
	name!: string

	@Field(() => String)
	email!: string

	@Field(() => String, { nullable: true })
	bio?: string
}

@ObjectType()
export class Post {
	@Field(() => String)
	id!: string

	@Field(() => String)
	title!: string

	@Field(() => String)
	content!: string

	@Field(() => Boolean)
	published!: boolean

	@Field(() => String)
	authorId!: string
}
```

These classes are ready to use with TypeGraphQL resolvers and provide full type safety for your GraphQL API.

### Resolver Implementation with Helpers

With the generated classes, you can create efficient resolvers using the provided helper utilities:

```typescript
import { Resolver, Query, Mutation, Arg, ID, Info, Ctx } from 'type-graphql'
import { GraphQLResolveInfo } from 'graphql'
import { User, UserConnection, UserFilterInput, UserSortInput } from '../schema'
import { buildConnection, buildFilter, buildSort, buildSelect } from '@hakutakuai/zenstack-graphql/helpers'
import { USER_WITH_POSTS_SELECT } from '../select-definitions'

@Resolver(() => User)
export class UserResolver {
	// Query with pagination, filtering, and sorting
	@Query(() => UserConnection)
	async users(
		@Arg('filter', () => UserFilterInput, { nullable: true }) filter: UserFilterInput | undefined,
		@Arg('sort', () => UserSortInput, { nullable: true }) sort: UserSortInput | undefined,
		@Arg('first', () => Int, { nullable: true }) first: number | undefined,
		@Arg('after', () => String, { nullable: true }) after: string | undefined,
		@Arg('last', () => Int, { nullable: true }) last: number | undefined,
		@Arg('before', () => String, { nullable: true }) before: string | undefined,
		@Info() info: GraphQLResolveInfo,
		@Ctx() { prisma }: Context,
	): Promise<UserConnection> {
		// Convert GraphQL filter to Prisma where
		const where = buildFilter(filter as any)

		// Convert GraphQL sort to Prisma orderBy
		const orderBy = buildSort(sort as any)

		// Get Prisma select based on requested GraphQL fields
		const select = buildSelect(USER_WITH_POSTS_SELECT, info)

		// Build connection config for pagination
		const config = buildConnection({
			first,
			after,
			last,
			before,
			where,
			orderBy,
			select,
		})

		// Execute the query with pagination
		const [items, totalCount] = await Promise.all([prisma.user.findMany(config.findMany), prisma.user.count(config.count)])

		// Convert results to a Connection format
		return config.toConnection(items, totalCount) as UserConnection
	}
}
```

## Helper Utilities Reference

Here's a comprehensive reference of the helper functions:

### `buildConnection`

Creates a Relay-compatible connection for pagination with cursor support.

```typescript
const config = buildConnection({
  // Pagination params
  first?: number;          // Number of items to fetch
  after?: string;          // Cursor to start after
  last?: number;           // Number of items from the end
  before?: string;         // Cursor to end before

  // Optional Prisma params
  where?: any;             // Prisma where condition
  orderBy?: any;           // Prisma ordering
  select?: any;            // Prisma field selection
  include?: any;           // Prisma include relations

  // Options
  defaultPageSize?: number;  // Default page size
  maxPageSize?: number;      // Maximum allowed page size
})

// Returns an object with:
// - findMany: Prisma args for fetching items
// - count: Prisma args for counting total items
// - toConnection: Function to convert results to a Connection
```

### `buildFilter`

Converts GraphQL filter input to Prisma where conditions.

```typescript
const where = buildFilter(filterInput)
```

### `buildSort`

Converts GraphQL sort input to Prisma orderBy.

```typescript
const orderBy = buildSort(sortInput)
```

### `buildSelect`

Creates a Prisma select object based on requested GraphQL fields.

```typescript
const select = buildSelect(baseSelect, graphqlInfo)
```

### `QueryBuilder`

A comprehensive builder for constructing complex Prisma queries.

```typescript
import { QueryBuilder } from '@hakutakuai/zenstack-graphql/helpers'

const query = new QueryBuilder().filter(filterInput).sort(sortInput).paginate({ first, after, last, before }).select(baseSelect, graphqlInfo).build()

const results = await prisma.model.findMany(query)
```

## Complete Server Setup Example

Below is a complete example of setting up a GraphQL server using the generated TypeGraphQL classes and helpers:

```typescript
// src/server.ts
import 'reflect-metadata'
import { createYoga } from 'graphql-yoga'
import { buildSchema } from 'type-graphql'
import { createServer } from 'node:http'
import { PrismaClient } from '@prisma/client'
import { resolvers } from './resolvers'

async function main() {
	// Create Prisma client
	const prisma = new PrismaClient()

	// Create TypeGraphQL schema
	const schema = await buildSchema({
		resolvers,
		validate: false,
		emitSchemaFile: './generated-schema.graphql',
	})

	// Create Yoga server
	const yoga = createYoga({
		schema,
		context: { prisma }, // Make Prisma available in resolvers
	})

	// Create HTTP server
	const server = createServer(yoga)

	// Start server
	server.listen(4000, () => {
		console.log('Server is running on http://localhost:4000/graphql')
	})
}

main().catch(console.error)
```

```typescript
// src/resolvers/index.ts
import { UserResolver } from './user.resolver'
import { PostResolver } from './post.resolver'
// Import other resolvers...

// Export all resolvers for TypeGraphQL schema building
export const resolvers = [
	UserResolver,
	PostResolver,
	// Add other resolvers...
] as const
```

## Best Practices

Here are some best practices for using the ZenStack GraphQL plugin:

### 1. Organize Select Definitions

Keep your select definitions in a separate file and organize them by model. This makes it easier to reuse select objects across resolvers.

```typescript
// src/select-definitions.ts
import type { Prisma } from '@prisma/client'

// Group by model
export const USER_BASE_SELECT = {...}
export const USER_WITH_POSTS_SELECT = {...}

export const POST_BASE_SELECT = {...}
export const POST_WITH_COMMENTS_SELECT = {...}
```

### 2. Use TypedPayloads for Return Types

Define types for your select objects to ensure type safety in your resolvers:

```typescript
export type UserWithPosts = Prisma.UserGetPayload<{
  select: typeof USER_WITH_POSTS_SELECT
}>

// In your resolver:
async function getUser(): Promise<UserWithPosts> {
  return prisma.user.findUnique({...}) as UserWithPosts
}
```

### 3. Combine Helpers for Maximum Efficiency

Combine multiple helpers to handle complex query requirements:

```typescript
const where = buildFilter(filter)
const orderBy = buildSort(sort)
const select = buildSelect(baseSelect, info)
const config = buildConnection({ where, orderBy, select, first, after })
```

### 4. Use QueryBuilder for Complex Queries

For more complex queries, use the QueryBuilder to construct your Prisma arguments:

```typescript
const query = new QueryBuilder().filter(filter).sort(sort).paginate({ first, after }).select(baseSelect, info).build()

const results = await prisma.model.findMany(query)
```

### 5. Handle Errors Gracefully

Add error handling to your resolvers to provide meaningful feedback:

```typescript
@Query(() => UserConnection)
async users(@Ctx() ctx: Context): Promise<UserConnection> {
  try {
    // Your implementation
  } catch (error) {
    console.error('Failed to fetch users:', error)
    throw new Error('Failed to fetch users. Please try again later.')
  }
}
```

> Check the [examples](./examples/) directory for more sample use cases!

## Contributing

Liked this plugin? Want to make it better? Contributions are always welcome! You can open a issue or pull requests on this repository 🧡

## License

This project is licensed under the GNU General Public License v3.0 (GPL-3.0) - see the [LICENSE](LICENSE) file for details.

### Built with 🧡 by the Hakutaku team.
