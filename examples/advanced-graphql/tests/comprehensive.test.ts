import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'bun:test'
import { fetch } from 'bun'
import { server, prismaClient } from '../src/server'

const TEST_PORT = 4569
const GRAPHQL_ENDPOINT = `http://localhost:${TEST_PORT}/graphql`

describe('Advanced GraphQL Comprehensive Tests', () => {
	beforeAll(() => {
		server.listen(TEST_PORT, () => {
			console.log(`Advanced GraphQL comprehensive test server running at http://localhost:${TEST_PORT}/graphql`)
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
		await prismaClient.productTag.deleteMany()
		await prismaClient.review.deleteMany()
		await prismaClient.product.deleteMany()
		await prismaClient.tag.deleteMany()
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

			expect(typeNames).toContain('Product')
			expect(typeNames).toContain('Review')
			expect(typeNames).toContain('Tag')
			expect(typeNames).toContain('ProductTag')
			expect(typeNames).toContain('ProductStatus')
			expect(typeNames).toContain('ReviewRating')
			expect(typeNames).toContain('ProductConnection')
			expect(typeNames).toContain('ReviewConnection')
			expect(typeNames).toContain('TagConnection')
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

			expect(queryFields).toContain('products')
			expect(queryFields).toContain('product')
			expect(queryFields).toContain('reviews')
			expect(queryFields).toContain('review')
			expect(queryFields).toContain('tags')
			expect(queryFields).toContain('tag')
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

			expect(mutationFields).toContain('createProduct')
			expect(mutationFields).toContain('updateProduct')
			expect(mutationFields).toContain('deleteProduct')
			expect(mutationFields).toContain('createReview')
			expect(mutationFields).toContain('createTag')
			expect(mutationFields).toContain('assignTagToProduct')
			expect(mutationFields).toContain('removeTagFromProduct')
		})

		test('Enums are properly registered', async () => {
			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query IntrospectEnums {
							__type(name: "ProductStatus") {
								enumValues {
									name
								}
							}
						}
					`,
				}),
			})

			const result = await response.json()
			const enumValues = result.data.__type.enumValues.map((e: any) => e.name)

			expect(enumValues).toContain('DRAFT')
			expect(enumValues).toContain('PUBLISHED')
			expect(enumValues).toContain('ARCHIVED')
		})
	})

	describe('Advanced Connection Builder Tests', () => {
		test('Products connection with complex filtering', async () => {
			await Promise.all([
				prismaClient.product.create({ data: { name: 'Expensive Laptop', price: 1500.0, status: 'PUBLISHED' } }),
				prismaClient.product.create({ data: { name: 'Budget Phone', price: 200.0, status: 'DRAFT' } }),
				prismaClient.product.create({ data: { name: 'Premium Tablet', price: 800.0, status: 'PUBLISHED' } }),
				prismaClient.product.create({ data: { name: 'Gaming Console', price: 500.0, status: 'ARCHIVED' } }),
			])

			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query ComplexProductFiltering {
							products(
								filter: {
									AND: [
										{ price: { gte: 500.0 } }
										{ status: { in: [PUBLISHED, ARCHIVED] } }
									]
								}
								sort: { price: DESC }
								first: 10
							) {
								totalCount
								pageInfo {
									hasNextPage
								}
								edges {
									cursor
									node {
										name
										price
										status
									}
								}
							}
						}
					`,
				}),
			})

			const result = await response.json()
			expect(result.errors).toBeUndefined()
			expect(result.data.products.totalCount).toBe(3)
			expect(result.data.products.edges[0].node.name).toBe('Expensive Laptop')
			expect(result.data.products.edges[1].node.name).toBe('Premium Tablet')
			expect(result.data.products.edges[2].node.name).toBe('Gaming Console')
		})

		test('Reviews connection with rating-based filtering', async () => {
			const product = await prismaClient.product.create({
				data: { name: 'Test Product', price: 100.0, status: 'PUBLISHED' },
			})

			await Promise.all([
				prismaClient.review.create({
					data: { title: 'Excellent!', content: 'Great product', rating: 'FIVE', verified: true, helpfulCount: 10, productId: product.id },
				}),
				prismaClient.review.create({
					data: { title: 'Good', content: 'Nice product', rating: 'FOUR', verified: false, helpfulCount: 5, productId: product.id },
				}),
				prismaClient.review.create({
					data: { title: 'Average', content: 'Okay product', rating: 'THREE', verified: true, helpfulCount: 2, productId: product.id },
				}),
			])

			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query HighRatedReviews {
							reviews(
								filter: {
									AND: [
										{ rating: { in: [FOUR, FIVE] } }
										{ verified: { equals: true } }
									]
								}
								sort: { helpfulCount: DESC }
							) {
								totalCount
								edges {
									node {
										title
										rating
										verified
										helpfulCount
									}
								}
							}
						}
					`,
				}),
			})

			const result = await response.json()
			expect(result.errors).toBeUndefined()
			expect(result.data.reviews.totalCount).toBe(1)
			expect(result.data.reviews.edges[0].node.title).toBe('Excellent!')
			expect(result.data.reviews.edges[0].node.rating).toBe('FIVE')
		})

		test('Tag connection with name-based filtering', async () => {
			await Promise.all([
				prismaClient.tag.create({ data: { name: 'Electronics', color: '#FF0000' } }),
				prismaClient.tag.create({ data: { name: 'Gadgets', color: '#00FF00' } }),
				prismaClient.tag.create({ data: { name: 'Mobile', color: '#0000FF' } }),
				prismaClient.tag.create({ data: { name: 'Computing' } }),
			])

			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query SearchTags {
							tags(
								filter: { name: { contains: "e" } }
								sort: { _placeholder: ASC }
								first: 10
							) {
								totalCount
								edges {
									node {
										name
										color
									}
								}
							}
						}
					`,
				}),
			})

			const result = await response.json()
			expect(result.errors).toBeUndefined()
			expect(result.data.tags.totalCount).toBe(3)
		})

		test('Cursor-based pagination works correctly', async () => {
			const products = await Promise.all([
				prismaClient.product.create({ data: { name: 'Product A', price: 100.0, status: 'PUBLISHED' } }),
				prismaClient.product.create({ data: { name: 'Product B', price: 200.0, status: 'PUBLISHED' } }),
				prismaClient.product.create({ data: { name: 'Product C', price: 300.0, status: 'PUBLISHED' } }),
				prismaClient.product.create({ data: { name: 'Product D', price: 400.0, status: 'PUBLISHED' } }),
			])

			const firstPageResponse = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query FirstPage {
							products(first: 2, sort: { name: ASC }) {
								pageInfo {
									hasNextPage
									endCursor
								}
								edges {
									cursor
									node {
										id
										name
									}
								}
							}
						}
					`,
				}),
			})

			const firstPage = await firstPageResponse.json()
			expect(firstPage.data.products.pageInfo.hasNextPage).toBe(true)

			const cursor = firstPage.data.products.pageInfo.endCursor
			const secondPageResponse = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query SecondPage($after: String!) {
							products(first: 2, after: $after, sort: { name: ASC }) {
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
					variables: { after: cursor },
				}),
			})

			const secondPage = await secondPageResponse.json()
			expect(secondPage.data.products.edges[0].node.name).toBe('Product C')
			expect(secondPage.data.products.edges[1].node.name).toBe('Product D')

			expect(secondPage.data.products.pageInfo.hasNextPage).toBe(false)
		})
	})

	describe('Advanced Relationship Tests', () => {
		test('Product with tags and reviews relationship resolution', async () => {
			const product = await prismaClient.product.create({
				data: { name: 'Test Product', price: 100.0, status: 'PUBLISHED', description: 'A test product' },
			})
			const tag1 = await prismaClient.tag.create({ data: { name: 'Electronics', color: '#FF0000' } })
			const tag2 = await prismaClient.tag.create({ data: { name: 'Gadgets' } })

			await prismaClient.productTag.create({ data: { productId: product.id, tagId: tag1.id } })
			await prismaClient.productTag.create({ data: { productId: product.id, tagId: tag2.id } })

			await prismaClient.review.create({
				data: { title: 'Great!', content: 'Excellent product', rating: 'FIVE', verified: true, helpfulCount: 5, productId: product.id },
			})

			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query ProductWithRelationships($id: ID!) {
							product(id: $id) {
								name
								description
								price
								tags {
									tag {
										name
										color
									}
									assignedAt
								}
								reviews {
									title
									rating
									verified
								}
							}
						}
					`,
					variables: { id: product.id },
				}),
			})

			const result = await response.json()
			expect(result.errors).toBeUndefined()

			const productData = result.data.product
			expect(productData.name).toBe('Test Product')
			expect(productData.tags).toHaveLength(2)
			expect(productData.reviews).toHaveLength(1)

			const tagNames = productData.tags.map((t: any) => t.tag.name)
			expect(tagNames).toContain('Electronics')
			expect(tagNames).toContain('Gadgets')
			expect(productData.reviews[0].title).toBe('Great!')
		})

		test('Tag to products relationship works correctly', async () => {
			const tag = await prismaClient.tag.create({ data: { name: 'Electronics' } })
			const product1 = await prismaClient.product.create({ data: { name: 'Laptop', price: 1000.0, status: 'PUBLISHED' } })
			const product2 = await prismaClient.product.create({ data: { name: 'Phone', price: 500.0, status: 'PUBLISHED' } })

			await prismaClient.productTag.create({ data: { productId: product1.id, tagId: tag.id } })
			await prismaClient.productTag.create({ data: { productId: product2.id, tagId: tag.id } })

			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query TagWithProducts($id: ID!) {
							tag(id: $id) {
								name
								products {
									product {
										name
										price
									}
								}
							}
						}
					`,
					variables: { id: tag.id },
				}),
			})

			const result = await response.json()
			expect(result.errors).toBeUndefined()
			expect(result.data.tag.products).toHaveLength(2)
		})

		test('Review to product relationship works correctly', async () => {
			const product = await prismaClient.product.create({
				data: { name: 'Test Product', price: 100.0, status: 'PUBLISHED' },
			})
			const review = await prismaClient.review.create({
				data: { title: 'Great!', content: 'Nice product', rating: 'FOUR', verified: true, helpfulCount: 3, productId: product.id },
			})

			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query ReviewWithProduct($id: ID!) {
							review(id: $id) {
								title
								rating
								product {
									name
									price
								}
							}
						}
					`,
					variables: { id: review.id },
				}),
			})

			const result = await response.json()
			expect(result.errors).toBeUndefined()
			expect(result.data.review.product.name).toBe('Test Product')
		})
	})

	describe('Mutation Tests', () => {
		test('Product lifecycle (create, update, delete)', async () => {
			const createResponse = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						mutation CreateProduct($input: ProductCreateInput!) {
							createProduct(input: $input) {
								id
								name
								price
								status
								description
							}
						}
					`,
					variables: {
						input: {
							name: 'Test Product',
							price: 99.99,
							status: 'DRAFT',
							description: 'A test product',
						},
					},
				}),
			})

			const createResult = await createResponse.json()
			expect(createResult.errors).toBeUndefined()

			const productId = createResult.data.createProduct.id
			expect(createResult.data.createProduct.name).toBe('Test Product')
			expect(createResult.data.createProduct.status).toBe('DRAFT')

			const updateResponse = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						mutation UpdateProduct($id: ID!, $input: ProductUpdateInput!) {
							updateProduct(id: $id, input: $input) {
								id
								name
								status
								price
							}
						}
					`,
					variables: {
						id: productId,
						input: {
							id: productId,
							createdAt: new Date().toISOString(),
							updatedAt: new Date().toISOString(),
							name: 'Test Product',
							status: 'PUBLISHED',
							price: 129.99,
						},
					},
				}),
			})

			const updateResult = await updateResponse.json()
			expect(updateResult.errors).toBeUndefined()
			expect(updateResult.data.updateProduct.status).toBe('PUBLISHED')
			expect(updateResult.data.updateProduct.price).toBe(129.99)

			const deleteResponse = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						mutation DeleteProduct($id: ID!) {
							deleteProduct(id: $id)
						}
					`,
					variables: { id: productId },
				}),
			})

			const deleteResult = await deleteResponse.json()
			expect(deleteResult.errors).toBeUndefined()
			expect(deleteResult.data.deleteProduct).toBe(true)

			const queryResponse = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query GetProduct($id: ID!) {
							product(id: $id) {
								id
							}
						}
					`,
					variables: { id: productId },
				}),
			})

			const queryResult = await queryResponse.json()
			expect(queryResult.data.product).toBeNull()
		})

		test('Tag assignment and removal', async () => {
			const product = await prismaClient.product.create({
				data: { name: 'Test Product', price: 100.0, status: 'PUBLISHED' },
			})
			const tag = await prismaClient.tag.create({ data: { name: 'Electronics' } })

			const assignResponse = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						mutation AssignTag($tagId: ID!, $productId: ID!) {
							assignTagToProduct(tagId: $tagId, productId: $productId)
						}
					`,
					variables: { tagId: tag.id, productId: product.id },
				}),
			})

			const assignResult = await assignResponse.json()
			expect(assignResult.errors).toBeUndefined()
			expect(assignResult.data.assignTagToProduct).toBe(true)

			const verifyResponse = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query VerifyAssignment($id: ID!) {
							product(id: $id) {
								tags {
									tag {
										name
									}
								}
							}
						}
					`,
					variables: { id: product.id },
				}),
			})

			const verifyResult = await verifyResponse.json()
			expect(verifyResult.data.product.tags).toHaveLength(1)
			expect(verifyResult.data.product.tags[0].tag.name).toBe('Electronics')

			const removeResponse = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						mutation RemoveTag($tagId: ID!, $productId: ID!) {
							removeTagFromProduct(tagId: $tagId, productId: $productId)
						}
					`,
					variables: { tagId: tag.id, productId: product.id },
				}),
			})

			const removeResult = await removeResponse.json()
			expect(removeResult.errors).toBeUndefined()
			expect(removeResult.data.removeTagFromProduct).toBe(true)
		})
	})

	describe('Error Handling and Edge Cases', () => {
		test('Handles invalid enum values', async () => {
			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						mutation CreateInvalidProduct {
							createProduct(input: {
								name: "Test"
								price: 100.0
								status: INVALID_STATUS
							}) {
								id
							}
						}
					`,
				}),
			})

			const result = await response.json()
			expect(result.errors).toBeDefined()
			expect(result.errors[0].message).toContain('Value "INVALID_STATUS"')
		})

		test('Handles non-existent entity operations', async () => {
			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query NonExistentProduct {
							product(id: "non-existent-id") {
								name
							}
						}
					`,
				}),
			})

			const result = await response.json()
			expect(result.errors).toBeUndefined()
			expect(result.data.product).toBeNull()
		})

		test('Handles empty filter results', async () => {
			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query EmptyResults {
							products(filter: { name: { contains: "NonExistentProduct" } }) {
								totalCount
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
			expect(result.data.products.totalCount).toBe(0)
			expect(result.data.products.edges).toHaveLength(0)
		})

		test('Handles large pagination requests', async () => {
			for (let i = 0; i < 20; i++) {
				await prismaClient.product.create({
					data: {
						name: `Product ${i + 1}`,
						price: (i + 1) * 10.0,
						status: i % 2 === 0 ? 'PUBLISHED' : 'DRAFT',
					},
				})
			}

			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query LargePageRequest {
							products(first: 15, sort: { name: ASC }) {
								totalCount
								pageInfo {
									hasNextPage
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
			expect(result.data.products.totalCount).toBe(20)
			expect(result.data.products.edges).toHaveLength(15)
			expect(result.data.products.pageInfo.hasNextPage).toBe(true)
		})
	})

	describe('Performance and Consistency Tests', () => {
		test('Consistent cursor generation', async () => {
			await Promise.all([
				prismaClient.product.create({ data: { name: 'Product A', price: 100.0, status: 'PUBLISHED' } }),
				prismaClient.product.create({ data: { name: 'Product B', price: 200.0, status: 'PUBLISHED' } }),
			])

			const response1 = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query GetCursors {
							products(first: 2, sort: { name: ASC }) {
								edges {
									cursor
									node { name }
								}
							}
						}
					`,
				}),
			})

			const response2 = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query GetCursors {
							products(first: 2, sort: { name: ASC }) {
								edges {
									cursor
									node { name }
								}
							}
						}
					`,
				}),
			})

			const result1 = await response1.json()
			const result2 = await response2.json()

			expect(result1.data.products.edges[0].cursor).toBe(result2.data.products.edges[0].cursor)
			expect(result1.data.products.edges[1].cursor).toBe(result2.data.products.edges[1].cursor)
		})

		test('Field resolver performance with multiple products', async () => {
			const products = await Promise.all([
				prismaClient.product.create({ data: { name: 'Product 1', price: 100.0, status: 'PUBLISHED' } }),
				prismaClient.product.create({ data: { name: 'Product 2', price: 200.0, status: 'PUBLISHED' } }),
				prismaClient.product.create({ data: { name: 'Product 3', price: 300.0, status: 'PUBLISHED' } }),
			])

			for (const product of products) {
				const tag = await prismaClient.tag.create({ data: { name: `Tag-${product.id}` } })
				await prismaClient.productTag.create({ data: { productId: product.id, tagId: tag.id } })
			}

			const startTime = Date.now()
			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query ProductsWithTags {
							products(first: 10) {
								edges {
									node {
										name
										tags {
											tag {
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
			const endTime = Date.now()

			const result = await response.json()
			expect(result.errors).toBeUndefined()
			expect(result.data.products.edges).toHaveLength(3)

			expect(endTime - startTime).toBeLessThan(1000)
		})
	})
})
