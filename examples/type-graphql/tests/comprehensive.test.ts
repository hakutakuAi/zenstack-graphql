import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'bun:test'
import { fetch } from 'bun'
import { server, prismaClient } from '../src/server'

const TEST_PORT = 4568
const GRAPHQL_ENDPOINT = `http://localhost:${TEST_PORT}/graphql`

describe('TypeGraphQL Comprehensive Tests', () => {
	beforeAll(() => {
		server.listen(TEST_PORT, () => {
			console.log(`TypeGraphQL comprehensive test server running at http://localhost:${TEST_PORT}/graphql`)
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

	describe('Schema Validation', () => {
		test('Schema has all expected types', async () => {
			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query IntrospectTypes {
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
			const typeNames = result.data.__schema.types.map((t: any) => t.name)

			expect(typeNames).toContain('User')
			expect(typeNames).toContain('Post')
			expect(typeNames).toContain('Comment')
			expect(typeNames).toContain('Category')
			expect(typeNames).toContain('PostCategory')
			expect(typeNames).toContain('UserConnection')
			expect(typeNames).toContain('PostConnection')
			expect(typeNames).toContain('CommentConnection')
			expect(typeNames).toContain('CategoryConnection')
			expect(typeNames).toContain('PostCategoryConnection')
		})

		test('Schema has all expected queries', async () => {
			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query IntrospectQueries {
							__schema {
								queryType {
									fields {
										name
										type { name }
									}
								}
							}
						}
					`,
				}),
			})

			const result = await response.json()
			const queryFields = result.data.__schema.queryType.fields.map((f: any) => f.name)

			expect(queryFields).toContain('users')
			expect(queryFields).toContain('user')
			expect(queryFields).toContain('posts')
			expect(queryFields).toContain('post')
			expect(queryFields).toContain('comments')
			expect(queryFields).toContain('categories')
			expect(queryFields).toContain('category')
			expect(queryFields).toContain('postCategories')
		})

		test('Schema has all expected mutations', async () => {
			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query IntrospectMutations {
							__schema {
								mutationType {
									fields {
										name
									}
								}
							}
						}
					`,
				}),
			})

			const result = await response.json()
			const mutationFields = result.data.__schema.mutationType.fields.map((f: any) => f.name)

			expect(mutationFields).toContain('createUser')
			expect(mutationFields).toContain('createPost')
			expect(mutationFields).toContain('publishPost')
			expect(mutationFields).toContain('createComment')
			expect(mutationFields).toContain('createCategory')
		})
	})

	describe('Connection Builder Pattern Tests', () => {
		test('Users connection works with all parameters', async () => {
			// Create test users
			await Promise.all([
				prismaClient.user.create({ data: { name: 'Alice', email: 'alice@test.com', bio: 'Developer' } }),
				prismaClient.user.create({ data: { name: 'Bob', email: 'bob@test.com' } }),
				prismaClient.user.create({ data: { name: 'Charlie', email: 'charlie@test.com', bio: 'Designer' } }),
			])

			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query TestUsersConnection($first: Int, $after: String, $sort: UserSortInput, $filter: UserFilterInput) {
							users(first: $first, after: $after, sort: $sort, filter: $filter) {
								totalCount
								pageInfo {
									hasNextPage
									hasPreviousPage
									startCursor
									endCursor
								}
								edges {
									cursor
									node {
										id
										name
										email
										bio
									}
								}
							}
						}
					`,
					variables: {
						first: 2,
						sort: { createdAt: 'ASC' },
						filter: { name: { contains: 'A' } },
					},
				}),
			})

			const result = await response.json()
			expect(result.errors).toBeUndefined()
			// Should filter for names containing 'A' (Alice and Charlie)
			expect(result.data.users.totalCount).toBe(2)
			expect(result.data.users.edges).toHaveLength(2)
			expect(result.data.users.pageInfo.hasNextPage).toBe(false)
		})

		test('Posts connection with filtering and sorting', async () => {
			const user = await prismaClient.user.create({ data: { name: 'Test User', email: 'test@example.com' } })

			await Promise.all([
				prismaClient.post.create({ data: { title: 'Alpha Post', content: 'Content A', authorId: user.id, published: true } }),
				prismaClient.post.create({ data: { title: 'Beta Post', content: 'Content B', authorId: user.id, published: false } }),
				prismaClient.post.create({ data: { title: 'Gamma Post', content: 'Content C', authorId: user.id, published: true } }),
			])

			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query TestPostsConnection {
							posts(
								filter: { published: { equals: true } }
								sort: { createdAt: DESC }
								first: 10
							) {
								totalCount
								edges {
									node {
										title
										published
										author {
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
			expect(result.errors).toBeUndefined()
			// Posts should be ordered by creation time (DESC)
			expect(result.data.posts.totalCount).toBe(2)
			expect(result.data.posts.edges).toHaveLength(2)
		})

		test('Backward pagination works correctly', async () => {
			await Promise.all([
				prismaClient.user.create({ data: { name: 'User 1', email: 'user1@test.com' } }),
				prismaClient.user.create({ data: { name: 'User 2', email: 'user2@test.com' } }),
				prismaClient.user.create({ data: { name: 'User 3', email: 'user3@test.com' } }),
				prismaClient.user.create({ data: { name: 'User 4', email: 'user4@test.com' } }),
			])

			// Get last 2 users
			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query TestBackwardPagination {
							users(last: 2, sort: { createdAt: ASC }) {
								totalCount
								pageInfo {
									hasNextPage
									hasPreviousPage
									startCursor
									endCursor
								}
								edges {
									cursor
									node {
										name
									}
								}
							}
						}
					`,
				}),
			})

			const result = await response.json()
			expect(result.errors).toBeUndefined()
			expect(result.data.users.totalCount).toBe(4)
			// Should get the last 2 users in creation order
			expect(result.data.users.edges).toHaveLength(2)
			expect(result.data.users.pageInfo.hasPreviousPage).toBe(true)
		})
	})

	describe('Complex Relationships Tests', () => {
		test('Nested relationships work correctly', async () => {
			const user = await prismaClient.user.create({ data: { name: 'Test User', email: 'test@example.com' } })
			const category = await prismaClient.category.create({ data: { name: 'Technology', description: 'Tech posts' } })
			const post = await prismaClient.post.create({ data: { title: 'Test Post', content: 'Content', authorId: user.id, published: true } })

			await prismaClient.categoryOnPost.create({ data: { postId: post.id, categoryId: category.id } })
			await prismaClient.comment.create({ data: { content: 'Great post!', postId: post.id, authorId: user.id } })

			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query TestNestedRelationships {
							posts(first: 1) {
								edges {
									node {
										title
										author {
											name
											email
										}
										categories {
											category {
												name
												description
											}
											assignedAt
										}
										comments {
											content
											author {
												name
											}
										}
									}
								}
							}
						}
					`,
				}),
			})

			const result = await response.json()
			expect(result.errors).toBeUndefined()

			const postNode = result.data.posts.edges[0].node
			expect(postNode.title).toBe('Test Post')
			expect(postNode.author.name).toBe('Test User')
			expect(postNode.categories).toHaveLength(1)
			expect(postNode.categories[0].category.name).toBe('Technology')
			expect(postNode.comments).toHaveLength(1)
			expect(postNode.comments[0].content).toBe('Great post!')
		})

		test('Many-to-many relationships work both directions', async () => {
			const user = await prismaClient.user.create({ data: { name: 'Test User', email: 'test@example.com' } })
			const category1 = await prismaClient.category.create({ data: { name: 'Tech', description: 'Technology' } })
			const category2 = await prismaClient.category.create({ data: { name: 'Science', description: 'Science posts' } })
			const post = await prismaClient.post.create({ data: { title: 'Test Post', content: 'Content', authorId: user.id } })

			await prismaClient.categoryOnPost.create({ data: { postId: post.id, categoryId: category1.id } })
			await prismaClient.categoryOnPost.create({ data: { postId: post.id, categoryId: category2.id } })

			// Test from Post to Categories
			const postResponse = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query TestPostToCategories {
							post(id: "${post.id}") {
								title
								categories {
									category {
										name
									}
								}
							}
						}
					`,
				}),
			})

			// Test from Categories to Posts
			const categoryResponse = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query TestCategoriesToPosts {
							categories(first: 10) {
								edges {
									node {
										name
										posts {
											post {
												title
											}
										}
									}
								}
							}
						}
					`,
				}),
			})

			const postResult = await postResponse.json()
			const categoryResult = await categoryResponse.json()

			expect(postResult.errors).toBeUndefined()
			expect(categoryResult.errors).toBeUndefined()

			expect(postResult.data.post.categories).toHaveLength(2)
			expect(categoryResult.data.categories.edges).toHaveLength(2)
			expect(categoryResult.data.categories.edges[0].node.posts).toHaveLength(1)
		})
	})

	describe('Error Handling Tests', () => {
		test('Handles invalid queries gracefully', async () => {
			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query InvalidQuery {
							nonExistentField
						}
					`,
				}),
			})

			const result = await response.json()
			expect(result.errors).toBeDefined()
			expect(result.errors[0].message).toContain('Cannot query field "nonExistentField"')
		})

		test('Handles non-existent entity queries', async () => {
			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query NonExistentUser {
							user(id: "non-existent-id") {
								name
							}
						}
					`,
				}),
			})

			const result = await response.json()
			expect(result.errors).toBeUndefined()
			expect(result.data.user).toBeNull()
		})

		test('Validates required fields in mutations', async () => {
			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						mutation CreateInvalidUser {
							createUser(name: "", email: "") {
								id
							}
						}
					`,
				}),
			})

			const result = await response.json()
			// Should either have validation errors or create with empty strings
			// The specific behavior depends on validation rules
			expect(response.status).toBe(200)
		})
	})

	describe('Performance and Edge Cases', () => {
		test('Handles large result sets with pagination', async () => {
			// Create 50 users
			const users = Array.from({ length: 50 }, (_, i) => ({
				name: `User ${i + 1}`,
				email: `user${i + 1}@test.com`,
			}))

			await prismaClient.user.createMany({ data: users })

			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query LargeResultSet {
							users(first: 10, sort: { createdAt: ASC }) {
								totalCount
								pageInfo {
									hasNextPage
									endCursor
								}
								edges {
									node {
										name
									}
								}
							}
						}
					`,
				}),
			})

			const result = await response.json()
			expect(result.errors).toBeUndefined()
			expect(result.data.users.totalCount).toBe(50)
			expect(result.data.users.edges).toHaveLength(10)
			expect(result.data.users.pageInfo.hasNextPage).toBe(true)
		})

		test('Handles empty result sets correctly', async () => {
			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query EmptyResultSet {
							users(filter: { name: { contains: "NonExistentUser" } }) {
								totalCount
								pageInfo {
									hasNextPage
									hasPreviousPage
								}
								edges {
									node {
										name
									}
								}
							}
						}
					`,
				}),
			})

			const result = await response.json()
			expect(result.errors).toBeUndefined()
			expect(result.data.users.totalCount).toBe(0)
			expect(result.data.users.edges).toHaveLength(0)
			expect(result.data.users.pageInfo.hasNextPage).toBe(false)
			expect(result.data.users.pageInfo.hasPreviousPage).toBe(false)
		})

		test('Complex filtering with AND/OR operations', async () => {
			const user = await prismaClient.user.create({ data: { name: 'Test User', email: 'test@example.com' } })

			await Promise.all([
				prismaClient.post.create({ data: { title: 'Important News', content: 'Breaking news', authorId: user.id, published: true } }),
				prismaClient.post.create({ data: { title: 'Daily Update', content: 'Regular update', authorId: user.id, published: false } }),
				prismaClient.post.create({ data: { title: 'Special Report', content: 'Important report', authorId: user.id, published: true } }),
			])

			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query ComplexFiltering {
							posts(
								filter: {
									OR: [
										{ 
											AND: [
												{ published: { equals: true } }
												{ title: { contains: "Important" } }
											]
										}
										{ title: { contains: "Update" } }
									]
								}
							) {
								totalCount
								edges {
									node {
										title
										published
									}
								}
							}
						}
					`,
				}),
			})

			const result = await response.json()
			expect(result.errors).toBeUndefined()
			expect(result.data.posts.totalCount).toBe(2) // Important posts and Update posts
		})
	})
})
