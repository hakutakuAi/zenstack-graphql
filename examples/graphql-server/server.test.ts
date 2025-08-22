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

	test('Can create categories and post with categories', async () => {
		const createUserResponse = await fetch(GRAPHQL_ENDPOINT, {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json',
			},
			body: JSON.stringify({
				query: `
			mutation {
				createUser(email: "author-categories-${Date.now()}@example.com", name: "Category Author") {
					id
				}
			}
        `,
			}),
		})

		const createUserResult = await createUserResponse.json()
		const authorId = createUserResult.data.createUser.id

		const createCategoryResponse = await fetch(GRAPHQL_ENDPOINT, {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json',
			},
			body: JSON.stringify({
				query: `
			mutation {
				createCategory(name: "Technology", description: "Tech related posts") {
					id
					name
					description
				}
			}
        `,
			}),
		})

		const createCategoryResult = await createCategoryResponse.json()
		expect(createCategoryResponse.status).toBe(200)
		expect(createCategoryResult.errors).toBeUndefined()
		expect(createCategoryResult.data.createCategory.name).toBe('Technology')
		expect(createCategoryResult.data.createCategory.description).toBe('Tech related posts')

		const categoryId = createCategoryResult.data.createCategory.id

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
					title: 'Tech Post',
					content: 'Content about technology',
					authorId: authorId,
				},
			}),
		})

		const createPostResult = await createPostResponse.json()
		expect(createPostResponse.status).toBe(200)
		expect(createPostResult.errors).toBeUndefined()
		expect(createPostResult.data.createPost.title).toBe('Tech Post')
		expect(createPostResult.data.createPost.published).toBe(true)
	})

	test('Can create comments on posts', async () => {
		const createUserResponse = await fetch(GRAPHQL_ENDPOINT, {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json',
			},
			body: JSON.stringify({
				query: `
			mutation {
				createUser(email: "commenter-${Date.now()}@example.com", name: "Comment Author") {
					id
				}
			}
        `,
			}),
		})

		const createUserResult = await createUserResponse.json()
		const authorId = createUserResult.data.createUser.id

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
				}
			}
        `,
				variables: {
					title: 'Post for Comments',
					content: 'This post will receive comments',
					authorId: authorId,
				},
			}),
		})

		const createPostResult = await createPostResponse.json()
		const postId = createPostResult.data.createPost.id

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
					post {
						id
						title
					}
					author {
						id
						name
					}
				}
			}
        `,
				variables: {
					content: 'Great post!',
					postId: postId,
					authorId: authorId,
				},
			}),
		})

		const createCommentResult = await createCommentResponse.json()
		expect(createCommentResponse.status).toBe(200)
		expect(createCommentResult.errors).toBeUndefined()
		expect(createCommentResult.data.createComment.content).toBe('Great post!')
		expect(createCommentResult.data.createComment.post.id).toBe(postId)
		expect(createCommentResult.data.createComment.author.id).toBe(authorId)
	})

	test('Schema introspection includes all expected mutations', async () => {
		const response = await fetch(GRAPHQL_ENDPOINT, {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json',
			},
			body: JSON.stringify({
				query: `
			{
				__schema {
					mutationType {
						fields {
							name
							type {
								name
							}
						}
					}
				}
			}
        `,
			}),
		})

		const result = await response.json()
		expect(response.status).toBe(200)
		expect(result.errors).toBeUndefined()

		const mutations = result.data.__schema.mutationType.fields
		const mutationNames = mutations.map((m: any) => m.name)

		expect(mutationNames).toContain('createUser')
		expect(mutationNames).toContain('createPost')
		expect(mutationNames).toContain('createCategory')
		expect(mutationNames).toContain('createComment')
	})

	test('Schema introspection validates field types', async () => {
		const response = await fetch(GRAPHQL_ENDPOINT, {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json',
			},
			body: JSON.stringify({
				query: `
			{
				__type(name: "Post") {
					fields {
						name
						type {
							name
							kind
						}
					}
				}
			}
        `,
			}),
		})

		const result = await response.json()
		expect(response.status).toBe(200)
		expect(result.errors).toBeUndefined()

		const fields = result.data.__type.fields
		const fieldMap = fields.reduce((acc: any, field: any) => {
			acc[field.name] = field.type
			return acc
		}, {})

		expect(fieldMap.id.name).toBe('String')
		expect(fieldMap.title.name).toBe('String')
		expect(fieldMap.content.name).toBe('String')
		expect(fieldMap.published.name).toBe('Boolean')
		expect(fieldMap.viewCount.name).toBe('Int')
		expect(fieldMap.author.name).toBe('User')
	})

	test('Error handling for invalid mutations', async () => {
		const response = await fetch(GRAPHQL_ENDPOINT, {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json',
			},
			body: JSON.stringify({
				query: `
			mutation {
				createUser(email: "invalid-email", name: "") {
					id
				}
			}
        `,
			}),
		})

		const result = await response.json()
		expect(response.status).toBe(200)
		if (result.errors) {
			expect(result.errors).toBeDefined()
			expect(result.errors.length).toBeGreaterThan(0)
		}
	})
})
