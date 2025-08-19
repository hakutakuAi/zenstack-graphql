import 'reflect-metadata'
import { buildSchema } from 'type-graphql'
import { createYoga } from 'graphql-yoga'
import { createServer } from 'node:http'
import { UserResolver, PostResolver } from './resolvers'

async function main() {
	try {
		const schema = await buildSchema({
			resolvers: [UserResolver, PostResolver],
			emitSchemaFile: true,
		})

		const yoga = createYoga({
			schema,
		})

		const server = createServer(yoga)

		const port = process.env.PORT || 4567

		server.listen(port, () => {
			console.log(`🔎 Explore the schema at http://localhost:${port}/graphql`)
		})
	} catch (error) {
		console.error('Error starting server:', error)
		process.exit(1)
	}
}

if (require.main === module) {
	main()
}
