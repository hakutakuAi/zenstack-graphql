import { describe, test, expect, beforeAll, afterAll } from 'bun:test'
import { fetch } from 'bun'
import { server, prismaClient } from './src/server'

const TEST_PORT = 4567
const GRAPHQL_ENDPOINT = `http://localhost:${TEST_PORT}/graphql`

describe('GraphQL Server Tests', () => {
	beforeAll(() => {
		server.listen(TEST_PORT, () => {
			console.log(`Test server running at http://localhost:${TEST_PORT}/graphql`)
		})

		return new Promise<void>((resolve) => {
			setTimeout(async () => {
				try {
					await fetch(GRAPHQL_ENDPOINT, {
						method: 'POST',
						headers: { 'Content-Type': 'application/json' },
						body: JSON.stringify({ query: '{ __typename }' }),
					})
					resolve()
				} catch (e) {
					console.log('Waiting for server to start...')
					setTimeout(resolve, 500)
				}
			}, 500)
		})
	})

	afterAll(() => {
		server.close()

		prismaClient.$disconnect()
	})

	test('GraphQL server provides a valid schema', async () => {
		const response = await fetch(GRAPHQL_ENDPOINT, {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json',
			},
			body: JSON.stringify({
				query: `
			{
				__schema {
				types {
					name
					kind
				}
				}
			}
        `,
			}),
		})

		const result = await response.json()

		expect(response.status).toBe(200)
		expect(result.errors).toBeUndefined()

		const types = result.data.__schema.types

		const typeNames = types.map((t: any) => t.name)
		expect(typeNames).toContain('User')
		expect(typeNames).toContain('Post')
		expect(typeNames).toContain('Category')
		expect(typeNames).toContain('Comment')

		expect(typeNames).toContain('Query')
		expect(typeNames).toContain('Mutation')
	})

	test('Can create a user and query it', async () => {
		const testEmail = `test-${Date.now()}@example.com`
		const testName = 'Test User'
		const testBio = 'Test Bio'

		const createResponse = await fetch(GRAPHQL_ENDPOINT, {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json',
			},
			body: JSON.stringify({
				query: `
			mutation CreateUser($email: String!, $name: String!, $bio: String) {
				createUser(email: $email, name: $name, bio: $bio) {
				id
				email
				name
				bio
				}
			}
        `,
				variables: {
					email: testEmail,
					name: testName,
					bio: testBio,
				},
			}),
		})

		const createResult = await createResponse.json()

		expect(createResponse.status).toBe(200)
		expect(createResult.errors).toBeUndefined()
		expect(createResult.data.createUser.email).toBe(testEmail)
		expect(createResult.data.createUser.name).toBe(testName)
		expect(createResult.data.createUser.bio).toBe(testBio)

		const userId = createResult.data.createUser.id

		const queryResponse = await fetch(GRAPHQL_ENDPOINT, {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json',
			},
			body: JSON.stringify({
				query: `
			query GetUser($id: String!) {
				user(id: $id) {
				id
				email
				name
				bio
				}
			}
        `,
				variables: {
					id: userId,
				},
			}),
		})

		const queryResult = await queryResponse.json()

		expect(queryResponse.status).toBe(200)
		expect(queryResult.errors).toBeUndefined()
		expect(queryResult.data.user.id).toBe(userId)
		expect(queryResult.data.user.email).toBe(testEmail)
	})

	test('Can create a post and query it', async () => {
		const createUserResponse = await fetch(GRAPHQL_ENDPOINT, {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json',
			},
			body: JSON.stringify({
				query: `
			mutation {
				createUser(email: "author-${Date.now()}@example.com", name: "Test Author") {
				id
				}
			}
        `,
			}),
		})

		const createUserResult = await createUserResponse.json()
		const authorId = createUserResult.data.createUser.id

		const testTitle = 'Test Post'
		const testContent = 'Test Content'

		const createPostResponse = await fetch(GRAPHQL_ENDPOINT, {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json',
			},
			body: JSON.stringify({
				query: `
			mutation CreatePost($title: String!, $content: String!, $authorId: String!) {
				createPost(title: $title, content: $content, published: true, authorId: $authorId) {
				id
				title
				content
				published
				author {
					id
					name
				}
				}
			}
        `,
				variables: {
					title: testTitle,
					content: testContent,
					authorId: authorId,
				},
			}),
		})

		const createPostResult = await createPostResponse.json()

		expect(createPostResponse.status).toBe(200)
		expect(createPostResult.errors).toBeUndefined()
		expect(createPostResult.data.createPost.title).toBe(testTitle)
		expect(createPostResult.data.createPost.content).toBe(testContent)
		expect(createPostResult.data.createPost.published).toBe(true)
		expect(createPostResult.data.createPost.author.id).toBe(authorId)
	})
})
