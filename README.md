# ZenStack GraphQL Plugin

A ZenStack plugin for automatically generating GraphQL schemas from your ZModel definitions. This plugin transforms your data models into a fully-featured GraphQL API schema with support for relations, filtering, pagination, and more.

[![License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

## 🌟 Features

- **Complete Type Generation**: Automatically generates GraphQL types from your ZModel schema
- **Relation Support**: Maintains model relationships in the GraphQL schema
- **Relay-Compatible**: Generates Relay-compliant connections for paginated queries
- **Filtering & Sorting**: Built-in support for filter and sort input types
- **Customization**: Extensive configuration options for type naming and field generation
- **Attributes**: Schema customization via ZModel attributes

## 📋 Table of Contents

- [Installation](#installation)
- [Usage](#usage)
- [Configuration](#configuration)
- [Schema Customization](#schema-customization)
- [Architecture](#architecture)
- [Examples](#examples)
- [Contributing](#contributing)
- [License](#license)

## 📦 Installation

```bash
# Using npm
npm install zenstack-graphql --save-dev

# Using yarn
yarn add zenstack-graphql -D

# Using bun
bun add zenstack-graphql -D
```

## 🚀 Usage

1. Add the plugin to your ZModel schema file:

```zmodel
plugin graphql {
    provider = 'zenstack-graphql'
    output = './schema.graphql'
}
```

2. Generate your schema:

```bash
zenstack generate
```

3. Use the generated GraphQL schema with your preferred GraphQL server implementation.

## ⚙️ Configuration

The plugin supports various configuration options to customize the generated schema:

```zmodel
plugin graphql {
    provider = 'zenstack-graphql'
    output = './schema.graphql'
    
    // Customize scalar type mappings
    scalarTypes = {
        DateTime: 'Date',
        Json: 'JSONObject'
    }
    
    // Control feature generation
    connectionTypes = true
    generateEnums = true
    generateScalars = true
    generateFilters = true
    generateSorts = true
    
    // Customize naming conventions
    fieldNaming = 'camelCase'  // 'camelCase', 'snake_case', or 'preserve'
    typeNaming = 'PascalCase'  // 'PascalCase', 'camelCase', or 'preserve'
    
    // Control relation handling
    includeRelations = true
}
```

### Configuration Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `output` | `string` | `'./schema.graphql'` | Output path for the generated GraphQL schema |
| `scalarTypes` | `object` | *See below* | Custom mappings for scalar types |
| `connectionTypes` | `boolean` | `true` | Generate Relay-compatible connection types |
| `generateEnums` | `boolean` | `true` | Generate GraphQL enum types |
| `generateScalars` | `boolean` | `true` | Generate GraphQL scalar types |
| `generateFilters` | `boolean` | `true` | Generate filter input types |
| `generateSorts` | `boolean` | `true` | Generate sort input types |
| `fieldNaming` | `string` | `'camelCase'` | Field naming convention |
| `typeNaming` | `string` | `'PascalCase'` | Type naming convention |
| `includeRelations` | `boolean` | `true` | Include model relations in schema |

Default scalar mappings:
```
{
  DateTime: 'DateTime',
  Json: 'JSON',
  Decimal: 'Decimal',
  Bytes: 'String'
}
```

## 🎨 Schema Customization

The plugin provides several attributes to customize the GraphQL schema at the model and field level.

### Model Attributes

```zmodel
model User {
  id Int @id
  
  // Ignore this model in GraphQL schema
  @@graphql.ignore
  
  // Set a custom name for this model
  @@graphql.name("Member")
  
  // Add a description for this model
  @@graphql.description("User account information")
  
  // Configure connection options
  @@graphql.connection(pageSize: 10)
}
```

### Field Attributes

```zmodel
model User {
  id Int @id
  
  // Ignore this field in GraphQL schema
  password String @graphql.ignore
  
  // Set a custom name for this field
  emailAddress String @graphql.name("email")
  
  // Add a description for this field
  fullName String @graphql.description("User's full name")
  
  // Make the field sortable in connection queries
  score Float @graphql.sortable
  
  // Make the field filterable in queries
  name String @graphql.filterable
}
```

## 🏗️ Architecture

The ZenStack GraphQL plugin transforms your ZModel schema into a GraphQL schema through several steps:

1. **Parsing**: Reads your ZModel schema and extracts models, enums, and relations.
2. **Type Generation**: Generates corresponding GraphQL types (objects, inputs, enums, scalars).
3. **Relation Processing**: Maps model relationships to GraphQL relations.
4. **Connection Generation**: Creates Relay-compatible connection types when enabled.
5. **Schema Composition**: Assembles all types into a complete GraphQL schema.
6. **SDL Output**: Writes the schema in SDL format to the specified output file.

### Key Components

- **Core Generator**: Orchestrates the schema generation process
- **Type Mapper**: Maps ZModel data types to GraphQL types
- **Registry**: Manages all GraphQL types and validates the final schema
- **Generators**: Specialized generators for different GraphQL type categories
  - Object Type Generator: Creates GraphQL object types from models
  - Relation Generator: Handles model relationships
  - Connection Generator: Builds Relay-compatible connection types
  - Filter/Sort Input Generators: Create input types for filtering and sorting

## 📝 Examples

### Basic Schema

```zmodel
datasource db {
  provider = "postgresql"
  url = env("DATABASE_URL")
}

generator js {
  provider = "prisma-client-js"
}

plugin graphql {
  provider = "zenstack-graphql"
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

This generates GraphQL types for `User` and `Post` with their relationships, along with connection types for pagination, and filter/sort inputs when those options are enabled.

Check the `/examples` directory for more sample use cases.

## 🤝 Contributing

Contributions are welcome! To contribute:

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.