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

## How It Works

The plugin transforms your ZModel schema into a GraphQL schema through these steps:

1. **Parsing**: Reads your data models and extracts entities, relationships, and attributes
2. **Type Generation**: Creates GraphQL types that match your data structure
3. **Relation Processing**: Maps your model relationships to GraphQL connections
4. **Connection Generation**: Builds Relay-compatible pagination support
5. **Schema Assembly**: Puts everything together into a complete GraphQL schema
6. **Output**: Writes the final schema to your specified location

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

> Check the [examples](./examples/) directory for more sample use cases!

## Contributing

Liked this plugin? Want to make it better? Contributions are always welcome! You can open a issue or pull requests on this repository 🧡

## License

This project is licensed under the GNU General Public License v3.0 (GPL-3.0) - see the [LICENSE](LICENSE) file for details.

### Built with 🧡 by the Hakutaku team.
