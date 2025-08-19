import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'bun:test'
import { fetch } from 'bun'
import { server, prismaClient } from './src/server'

const TEST_PORT = 4567
const GRAPHQL_ENDPOINT = `http://localhost:${TEST_PORT}/graphql`

describe('TypeGraphQL Server Tests', () => {
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

	beforeEach(async () => {
		await prismaClient.comment.deleteMany()
		await prismaClient.categoryOnPost.deleteMany()
		await prismaClient.post.deleteMany()
		await prismaClient.category.deleteMany()
		await prismaClient.user.deleteMany()
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

	test('Can create a post and query it with author relation', async () => {
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

	test('Can query users with their posts', async () => {
		const response = await fetch(GRAPHQL_ENDPOINT, {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json',
			},
			body: JSON.stringify({
				query: `
			query {
				users {
				id
				name
				email
				posts {
					id
					title
					published
				}
				}
			}
        `,
			}),
		})

		const result = await response.json()

		expect(response.status).toBe(200)
		expect(result.errors).toBeUndefined()
		expect(Array.isArray(result.data.users)).toBe(true)
	})

	test('Can create categories and comments', async () => {
		const uniqueCategoryName = `Test Category ${Date.now()}`
		const createCategoryResponse = await fetch(GRAPHQL_ENDPOINT, {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json',
			},
			body: JSON.stringify({
				query: `
			mutation CreateCategory($name: String!, $description: String) {
				createCategory(name: $name, description: $description) {
				id
				name
				description
				}
			}
        `,
				variables: {
					name: uniqueCategoryName,
					description: 'Test Description',
				},
			}),
		})

		const categoryResult = await createCategoryResponse.json()

		expect(createCategoryResponse.status).toBe(200)
		expect(categoryResult.errors).toBeUndefined()
		expect(categoryResult.data.createCategory.name).toBe(uniqueCategoryName)
		expect(categoryResult.data.createCategory.description).toBe('Test Description')

		const createUserResponse = await fetch(GRAPHQL_ENDPOINT, {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json',
			},
			body: JSON.stringify({
				query: `
			mutation {
				createUser(email: "commenter-${Date.now()}@example.com", name: "Test Commenter") {
				id
				}
			}
        `,
			}),
		})

		const userResult = await createUserResponse.json()

		const createPostResponse = await fetch(GRAPHQL_ENDPOINT, {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json',
			},
			body: JSON.stringify({
				query: `
			mutation CreatePost($authorId: String!) {
				createPost(title: "Post for Comments", content: "Content", authorId: $authorId) {
				id
				}
			}
        `,
				variables: {
					authorId: userResult.data.createUser.id,
				},
			}),
		})

		const postResult = await createPostResponse.json()

		const createCommentResponse = await fetch(GRAPHQL_ENDPOINT, {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json',
			},
			body: JSON.stringify({
				query: `
			mutation CreateComment($content: String!, $postId: String!, $authorId: String!) {
				createComment(content: $content, postId: $postId, authorId: $authorId) {
				id
				content
				author {
					id
					name
				}
				post {
					id
					title
				}
				}
			}
        `,
				variables: {
					content: 'Test Comment',
					postId: postResult.data.createPost.id,
					authorId: userResult.data.createUser.id,
				},
			}),
		})

		const commentResult = await createCommentResponse.json()

		expect(createCommentResponse.status).toBe(200)
		expect(commentResult.errors).toBeUndefined()
		expect(commentResult.data.createComment.content).toBe('Test Comment')
		expect(commentResult.data.createComment.author.name).toBe('Test Commenter')
		expect(commentResult.data.createComment.post.title).toBe('Post for Comments')
	})
})
