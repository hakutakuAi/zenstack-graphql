import 'reflect-metadata'
import { buildSchema } from 'type-graphql'
import { createYoga } from 'graphql-yoga'
import { createServer } from 'node:http'
import { PrismaClient } from '@prisma/client'
import { ProductResolver, ReviewResolver, TagResolver } from './resolvers'
import type { Context } from './resolvers/types'

const prisma = new PrismaClient()

const schema = buildSchema({
	resolvers: [ProductResolver, ReviewResolver, TagResolver],
	emitSchemaFile: false,
})

export function createContext(): Context {
	return {
		prisma,
	}
}

const yoga = createYoga({
	schema,
	context: () => createContext(),
	graphiql: {
		title: 'Advanced GraphQL Example',
		defaultQuery: `
# Welcome to the Advanced GraphQL Example!
# This example demonstrates advanced features like:
# - Relay-style connections and pagination
# - Complex filtering with AND/OR operations
# - JSON field support for flexible metadata
# - Multiple sorting options

query GetProducts {
  products(args: { 
    first: 5
    filter: {
      status: { equals: PUBLISHED }
      price: { gte: 10.0, lte: 100.0 }
    }
    sort: { price: ASC }
  }) {
    edges {
      node {
        id
        name
        price
        status
        metadata
        reviews {
          id
          title
          rating
          verified
        }
      }
      cursor
    }
    pageInfo {
      hasNextPage
      hasPreviousPage
      startCursor
      endCursor
    }
    totalCount
  }
}
		`
	}
})

export const server = createServer(yoga)
export const prismaClient = prisma

if (require.main === module) {
	const port = process.env.PORT || 4000

	server.listen(port, () => {
		console.log(`🚀 Advanced GraphQL server running at http://localhost:${port}/graphql`)
		console.log('Features available:')
		console.log('  • Relay-style connections and pagination')
		console.log('  • Complex filtering (AND/OR operations)')
		console.log('  • JSON metadata fields')
		console.log('  • Multiple sorting options')
		console.log('  • Enum support')
	})

	process.on('SIGINT', async () => {
		console.log('Shutting down gracefully...')
		await prisma.$disconnect()
		process.exit(0)
	})
}