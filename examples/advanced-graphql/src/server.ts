import 'reflect-metadata'
import { ProductResolver, ReviewResolver, TagResolver } from './resolvers'
import { buildSchema } from 'type-graphql'
import { createYoga } from 'graphql-yoga'
import { createServer } from 'node:http'
import { PrismaClient } from '@prisma/client'
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
})

export const server = createServer(yoga)
export const prismaClient = prisma

if (require.main === module) {
	const port = process.env.PORT || 4000

	server.listen(port, () => {
		console.log(`🔎 Explore the schema at http://localhost:${port}/graphql`)
	})

	process.on('SIGINT', async () => {
		console.log('Shutting down gracefully...')
		await prisma.$disconnect()
		process.exit(0)
	})
}
