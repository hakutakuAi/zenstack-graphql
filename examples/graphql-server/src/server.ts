import { createSchema, createYoga } from 'graphql-yoga'
import { createServer } from 'node:http'
import { readFileSync } from 'node:fs'
import { PrismaClient } from '@prisma/client'
import { resolve } from 'path'
import { resolvers } from './resolvers'

const prisma = new PrismaClient()

const generatedTypeDefs = readFileSync(resolve(__dirname, './schema.graphql'), 'utf-8')

const resolversDefs = /* GraphQL */ `
	type Query {
		user(id: String!): User
		users(first: Int, after: String): [User!]!

		post(id: String!): Post
		posts(first: Int, after: String, published: Boolean): [Post!]!

		category(id: String!): Category
		categories(first: Int, after: String): [Category!]!
	}

	type Mutation {
		createUser(email: String!, name: String!, bio: String): User!
		createPost(title: String!, content: String!, published: Boolean, authorId: String!): Post!
		createComment(content: String!, postId: String!, authorId: String!): Comment!
		createCategory(name: String!, description: String): Category!
		addCategoryToPost(postId: String!, categoryId: String!): PostCategory!
	}
`

const typeDefs = [generatedTypeDefs, resolversDefs]
const yoga = createYoga({
	schema: createSchema({
		typeDefs,
		resolvers,
	}),
	context: ({ request }) => {
		return {
			prisma,
			request,
		}
	},
})

export const server = createServer(yoga)
export const prismaClient = prisma

// Only start the server if this file is run directly
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
