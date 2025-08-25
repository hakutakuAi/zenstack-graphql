import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'bun:test'
import { fetch } from 'bun'
import { server, prismaClient } from '../src/server'

const TEST_PORT = 4568
const GRAPHQL_ENDPOINT = `http://localhost:${TEST_PORT}/graphql`

describe('TypeGraphQL Server Tests', () => {
	beforeAll(() => {
		server.listen(TEST_PORT, () => {
			console.log(`TypeGraphQL test server running at http://localhost:${TEST_PORT}/graphql`)
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
					edges {
						node {
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
					pageInfo {
						hasNextPage
					}
				}
			}
        `,
			}),
		})

		const result = await response.json()

		expect(response.status).toBe(200)
		expect(result.errors).toBeUndefined()
		expect(result.data.users).toBeDefined()
		expect(Array.isArray(result.data.users.edges)).toBe(true)
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

	describe('Filter Functionality', () => {
		test('Can filter users by email', async () => {
			const testEmail = `filter-test-${Date.now()}@example.com`
			await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `mutation { createUser(email: "${testEmail}", name: "Filter Test User") { id } }`,
				}),
			})

			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query FilterUsers($filter: UserFilterInput) {
							users(filter: $filter) {
								edges {
									node {
										id
										email
										name
									}
								}
								pageInfo {
									hasNextPage
								}
							}
						}
					`,
					variables: {
						filter: {
							email: { equals: testEmail },
						},
					},
				}),
			})

			const result = await response.json()
			expect(response.status).toBe(200)
			expect(result.data?.users).toBeDefined()
			expect(result.data.users.edges.length).toBeGreaterThanOrEqual(1)
			expect(result.data.users.edges[0].node.email).toBe(testEmail)
		})

		test('Can filter posts by published status', async () => {
			const createUserResponse = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `mutation { createUser(email: "post-filter-${Date.now()}@example.com", name: "Post Filter User") { id } }`,
				}),
			})
			const userResult = await createUserResponse.json()
			const authorId = userResult.data.createUser.id

			await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `mutation { createPost(title: "Published Post", content: "Content", authorId: "${authorId}", published: true) { id } }`,
				}),
			})

			await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `mutation { createPost(title: "Draft Post", content: "Content", authorId: "${authorId}", published: false) { id } }`,
				}),
			})

			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query FilterPosts($filter: PostFilterInput) {
							posts(filter: $filter) {
								edges {
									node {
										id
										title
										published
									}
								}
								pageInfo {
									hasNextPage
								}
							}
						}
					`,
					variables: {
						filter: {
							published: { equals: true },
						},
					},
				}),
			})

			const result = await response.json()
			expect(response.status).toBe(200)
			expect(result.data?.posts).toBeDefined()
			expect(result.data.posts.edges.length).toBeGreaterThanOrEqual(1)
			expect(result.data.posts.edges.every((edge: any) => edge.node.published)).toBe(true)
		})

		test('Can filter with string contains operation', async () => {
			const uniqueName = `StringFilter${Date.now()}`
			await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `mutation { createUser(email: "string-filter-${Date.now()}@example.com", name: "${uniqueName}") { id } }`,
				}),
			})

			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query FilterUsers($filter: UserFilterInput) {
							users(filter: $filter) {
								edges {
									node {
										id
										name
									}
								}
								pageInfo {
									hasNextPage
								}
							}
						}
					`,
					variables: {
						filter: {
							name: { contains: 'StringFilter' },
						},
					},
				}),
			})

			const result = await response.json()
			expect(response.status).toBe(200)
			expect(result.data?.users).toBeDefined()
			expect(result.data.users.edges.length).toBeGreaterThanOrEqual(1)
			expect(result.data.users.edges.some((edge: any) => edge.node.name.includes('StringFilter'))).toBe(true)
		})
	})

	describe('Sort Functionality', () => {
		test('Can sort users by createdAt in ascending order', async () => {
			const baseEmail = `sort-test-${Date.now()}`
			await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `mutation { createUser(email: "${baseEmail}-1@example.com", name: "Sort User 1") { id } }`,
				}),
			})

			await new Promise((resolve) => setTimeout(resolve, 100))

			await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `mutation { createUser(email: "${baseEmail}-2@example.com", name: "Sort User 2") { id } }`,
				}),
			})

			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query SortUsers($sort: UserSortInput) {
							users(sort: $sort) {
								edges {
									node {
										id
										name
										createdAt
									}
								}
								pageInfo {
									hasNextPage
								}
							}
						}
					`,
					variables: {
						sort: { createdAt: 'ASC' },
					},
				}),
			})

			const result = await response.json()
			expect(response.status).toBe(200)
			expect(result.data?.users).toBeDefined()
			expect(result.data.users.edges.length).toBeGreaterThanOrEqual(2)

			const dates = result.data.users.edges.map((edge: any) => new Date(edge.node.createdAt).getTime())
			for (let i = 1; i < dates.length; i++) {
				expect(dates[i]).toBeGreaterThanOrEqual(dates[i - 1])
			}
		})

		test('Can sort posts by viewCount in descending order', async () => {
			const createUserResponse = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `mutation { createUser(email: "sort-posts-${Date.now()}@example.com", name: "Sort Posts User") { id } }`,
				}),
			})
			const userResult = await createUserResponse.json()
			const authorId = userResult.data.createUser.id

			await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `mutation { createPost(title: "Low Views Post", content: "Content", authorId: "${authorId}") { id } }`,
				}),
			})

			await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `mutation { createPost(title: "High Views Post", content: "Content", authorId: "${authorId}") { id } }`,
				}),
			})

			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query SortPosts($sort: PostSortInput) {
							posts(sort: $sort) {
								edges {
									node {
										id
										title
										viewCount
									}
								}
								pageInfo {
									hasNextPage
								}
							}
						}
					`,
					variables: {
						sort: { viewCount: 'DESC' },
					},
				}),
			})

			const result = await response.json()
			expect(response.status).toBe(200)
			expect(result.data?.posts).toBeDefined()
			expect(result.data.posts.edges.length).toBeGreaterThanOrEqual(2)

			const viewCounts = result.data.posts.edges.map((edge: any) => edge.node.viewCount)
			for (let i = 1; i < viewCounts.length; i++) {
				expect(viewCounts[i]).toBeLessThanOrEqual(viewCounts[i - 1])
			}
		})
	})

	describe('Connection/Pagination Functionality', () => {
		test('Can query users with pagination (first/after)', async () => {
			for (let i = 0; i < 5; i++) {
				await fetch(GRAPHQL_ENDPOINT, {
					method: 'POST',
					headers: { 'Content-Type': 'application/json' },
					body: JSON.stringify({
						query: `mutation { createUser(email: "pagination-${i}-${Date.now()}@example.com", name: "Pagination User ${i}") { id } }`,
					}),
				})
			}

			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query PaginateUsers($first: Int, $after: String) {
							users(first: $first, after: $after) {
								pageInfo {
									hasNextPage
									hasPreviousPage
									startCursor
									endCursor
								}
								edges {
									node {
										id
										name
									}
									cursor
								}
								totalCount
							}
						}
					`,
					variables: {
						first: 2,
					},
				}),
			})

			const result = await response.json()
			expect(response.status).toBe(200)
			expect(result.data?.users).toBeDefined()
			expect(result.data.users.pageInfo).toBeDefined()
			expect(result.data.users.edges).toBeDefined()
			expect(result.data.users.edges.length).toBeLessThanOrEqual(2)
			expect(result.data.users.totalCount).toBeGreaterThanOrEqual(5)
			expect(typeof result.data.users.pageInfo.hasNextPage).toBe('boolean')
		})

		test('Can query posts connection with pagination', async () => {
			const createUserResponse = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `mutation { createUser(email: "connection-posts-${Date.now()}@example.com", name: "Connection Posts User") { id } }`,
				}),
			})
			const userResult = await createUserResponse.json()
			const authorId = userResult.data.createUser.id

			for (let i = 0; i < 3; i++) {
				await fetch(GRAPHQL_ENDPOINT, {
					method: 'POST',
					headers: { 'Content-Type': 'application/json' },
					body: JSON.stringify({
						query: `mutation { createPost(title: "Connection Post ${i}", content: "Content ${i}", authorId: "${authorId}") { id } }`,
					}),
				})
			}

			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query PaginatePosts($last: Int) {
							posts(last: $last) {
								pageInfo {
									hasNextPage
									hasPreviousPage
									startCursor
									endCursor
								}
								edges {
									node {
										id
										title
									}
									cursor
								}
								totalCount
							}
						}
					`,
					variables: {
						last: 2,
					},
				}),
			})

			const result = await response.json()
			expect(response.status).toBe(200)
			expect(result.data?.posts).toBeDefined()
			expect(result.data.posts.pageInfo).toBeDefined()
			expect(result.data.posts.edges).toBeDefined()
			expect(result.data.posts.edges.length).toBeLessThanOrEqual(2)
			expect(typeof result.data.posts.totalCount).toBe('number')
		})

		test('PageInfo provides correct pagination metadata', async () => {
			const createUserResponse = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `mutation { createUser(email: "pageinfo-${Date.now()}@example.com", name: "PageInfo User") { id } }`,
				}),
			})

			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query TestPageInfo {
							users(first: 1) {
								pageInfo {
									hasNextPage
									hasPreviousPage
									startCursor
									endCursor
								}
								edges {
									cursor
								}
							}
						}
					`,
				}),
			})

			const result = await response.json()
			expect(response.status).toBe(200)
			expect(result.data.users.pageInfo).toBeDefined()
			expect(typeof result.data.users.pageInfo.hasNextPage).toBe('boolean')
			expect(typeof result.data.users.pageInfo.hasPreviousPage).toBe('boolean')
			if (result.data.users.edges.length > 0) {
				expect(result.data.users.pageInfo.startCursor).toBeDefined()
				expect(result.data.users.pageInfo.endCursor).toBeDefined()
			}
		})
	})

	describe('Combined Filters and Sorts', () => {
		test('Can combine filters and sorts in a single query', async () => {
			const createUserResponse = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `mutation { createUser(email: "combined-${Date.now()}@example.com", name: "Combined User") { id } }`,
				}),
			})
			const userResult = await createUserResponse.json()
			const authorId = userResult.data.createUser.id

			await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `mutation { createPost(title: "Published Combined", content: "Content", authorId: "${authorId}", published: true) { id } }`,
				}),
			})

			await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `mutation { createPost(title: "Draft Combined", content: "Content", authorId: "${authorId}", published: false) { id } }`,
				}),
			})

			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query FilterAndSort($filter: PostFilterInput, $sort: PostSortInput) {
							posts(filter: $filter, sort: $sort) {
								edges {
									node {
										id
										title
										published
										createdAt
									}
								}
								pageInfo {
									hasNextPage
								}
							}
						}
					`,
					variables: {
						filter: {
							published: { equals: true },
						},
						sort: {
							createdAt: 'DESC',
						},
					},
				}),
			})

			const result = await response.json()
			expect(response.status).toBe(200)
			expect(result.data?.posts).toBeDefined()
			expect(result.data.posts.edges.every((edge: any) => edge.node.published)).toBe(true)
		})
	})
})
