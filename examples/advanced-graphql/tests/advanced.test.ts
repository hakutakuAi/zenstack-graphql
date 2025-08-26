import { describe, test, expect, beforeAll, afterAll, beforeEach } from 'bun:test'
import { fetch } from 'bun'
import { server, prismaClient } from '../src/server'

const TEST_PORT = 4569
const GRAPHQL_ENDPOINT = `http://localhost:${TEST_PORT}/graphql`

describe('Advanced GraphQL Features Tests', () => {
	beforeAll(() => {
		server.listen(TEST_PORT, () => {
			console.log(`Advanced GraphQL test server running at http://localhost:${TEST_PORT}/graphql`)
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

	describe('Schema Generation Tests', () => {
		test('Schema contains all expected types and enums', async () => {
			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
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
			expect(result.errors).toBeUndefined()

			const types = result.data.__schema.types
			const typeNames = types.map((t: any) => t.name)

			expect(typeNames).toContain('Product')
			expect(typeNames).toContain('Review')
			expect(typeNames).toContain('Tag')
			expect(typeNames).toContain('ProductTag')

			expect(typeNames).toContain('ProductConnection')
			expect(typeNames).toContain('ReviewConnection')
			expect(typeNames).toContain('TagConnection')

			expect(typeNames).toContain('ProductEdge')
			expect(typeNames).toContain('ReviewEdge')
			expect(typeNames).toContain('TagEdge')

			expect(typeNames).toContain('ProductFilterInput')
			expect(typeNames).toContain('ReviewFilterInput')
			expect(typeNames).toContain('TagFilterInput')

			expect(typeNames).toContain('ProductSortInput')
			expect(typeNames).toContain('ReviewSortInput')
			expect(typeNames).toContain('TagSortInput')

			expect(typeNames).toContain('ProductStatus')
			expect(typeNames).toContain('ReviewRating')
			expect(typeNames).toContain('SortDirection')

			expect(typeNames).toContain('NumericFilterInput')
			expect(typeNames).toContain('StringFilterInput')
			expect(typeNames).toContain('BooleanFilterInput')

			expect(typeNames).toContain('PageInfo')
		})
	})

	describe('JSON Field Support', () => {
		test('Can create and query products with JSON metadata', async () => {
			const productData = {
				name: 'Test Product',
				description: 'A test product',
				price: 29.99,
				status: 'PUBLISHED',
				metadata: {
					brand: 'TestBrand',
					features: ['feature1', 'feature2'],
					specifications: {
						weight: '1kg',
						dimensions: '10x10x5cm',
					},
				},
			}

			const createResponse = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						mutation CreateProduct($input: ProductCreateInput!) {
							createProduct(input: $input) {
								id
								name
								metadata
							}
						}
					`,
					variables: { input: productData },
				}),
			})

			const createResult = await createResponse.json()
			expect(createResult.errors).toBeUndefined()
			expect(createResult.data.createProduct.metadata).toEqual(productData.metadata)
		})
	})

	describe('Relay Connections and Pagination', () => {
		test('Products connection follows Relay specification', async () => {
			const products = await Promise.all([
				prismaClient.product.create({
					data: { name: 'Product A', price: 10.0, status: 'PUBLISHED' },
				}),
				prismaClient.product.create({
					data: { name: 'Product B', price: 20.0, status: 'PUBLISHED' },
				}),
				prismaClient.product.create({
					data: { name: 'Product C', price: 30.0, status: 'PUBLISHED' },
				}),
			])

			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query GetProducts($first: Int, $after: String) {
							products(args: { first: $first, after: $after }) {
								edges {
									node {
										id
										name
										price
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
					`,
					variables: { first: 2 },
				}),
			})

			const result = await response.json()
			expect(result.errors).toBeUndefined()

			const connection = result.data.products

			expect(connection.edges).toHaveLength(2)
			expect(connection.totalCount).toBe(3)
			expect(connection.pageInfo.hasNextPage).toBe(true)
			expect(connection.pageInfo.startCursor).toBeDefined()
			expect(connection.pageInfo.endCursor).toBeDefined()

			connection.edges.forEach((edge: any) => {
				expect(edge.node).toBeDefined()
				expect(edge.cursor).toBeDefined()
				expect(edge.node.id).toBeDefined()
			})
		})

		test('Forward pagination with cursors works correctly', async () => {
			await Promise.all([
				prismaClient.product.create({ data: { name: 'Product 1', price: 10.0, status: 'PUBLISHED' } }),
				prismaClient.product.create({ data: { name: 'Product 2', price: 20.0, status: 'PUBLISHED' } }),
				prismaClient.product.create({ data: { name: 'Product 3', price: 30.0, status: 'PUBLISHED' } }),
			])

			const firstPageResponse = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query GetProducts($first: Int) {
							products(args: { first: $first }) {
								edges {
									node { name }
									cursor
								}
								pageInfo {
									hasNextPage
									endCursor
								}
							}
						}
					`,
					variables: { first: 2 },
				}),
			})

			const firstPage = await firstPageResponse.json()
			expect(firstPage.data.products.pageInfo.hasNextPage).toBe(true)

			const endCursor = firstPage.data.products.pageInfo.endCursor

			const secondPageResponse = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query GetProducts($first: Int, $after: String) {
							products(args: { first: $first, after: $after }) {
								edges {
									node { name }
								}
								pageInfo {
									hasNextPage
								}
							}
						}
					`,
					variables: { first: 2, after: endCursor },
				}),
			})

			const secondPage = await secondPageResponse.json()
			expect(secondPage.data.products.edges).toHaveLength(1)
			expect(secondPage.data.products.pageInfo.hasNextPage).toBe(false)
		})
	})

	describe('Complex Filtering', () => {
		test('Simple field filtering works', async () => {
			await Promise.all([
				prismaClient.product.create({ data: { name: 'Expensive Item', price: 100.0, status: 'PUBLISHED' } }),
				prismaClient.product.create({ data: { name: 'Cheap Item', price: 10.0, status: 'PUBLISHED' } }),
				prismaClient.product.create({ data: { name: 'Draft Item', price: 50.0, status: 'DRAFT' } }),
			])

			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query FilterProducts {
							products(args: {
								filter: {
									status: { equals: PUBLISHED }
									price: { gte: 50.0 }
								}
							}) {
								edges {
									node {
										name
										price
										status
									}
								}
								totalCount
							}
						}
					`,
				}),
			})

			const result = await response.json()
			expect(result.errors).toBeUndefined()
			expect(result.data.products.totalCount).toBe(1)
			expect(result.data.products.edges[0]?.node.name).toBe('Expensive Item')
		})

		test('AND/OR filtering operations work', async () => {
			await Promise.all([
				prismaClient.product.create({ data: { name: 'Product A', price: 100.0, status: 'PUBLISHED' } }),
				prismaClient.product.create({ data: { name: 'Product B', price: 200.0, status: 'DRAFT' } }),
				prismaClient.product.create({ data: { name: 'Product C', price: 50.0, status: 'PUBLISHED' } }),
			])

			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query FilterProductsWithOR {
							products(args: {
								filter: {
									OR: [
										{ price: { gte: 150.0 } }
										{ 
											AND: [
												{ status: { equals: PUBLISHED } }
												{ price: { lte: 60.0 } }
											]
										}
									]
								}
							}) {
								edges {
									node {
										name
										price
										status
									}
								}
								totalCount
							}
						}
					`,
				}),
			})

			const result = await response.json()
			expect(result.errors).toBeUndefined()
			expect(result.data.products.totalCount).toBe(2)
		})
	})

	describe('Sorting', () => {
		test('Single field sorting works', async () => {
			await Promise.all([
				prismaClient.product.create({ data: { name: 'Z Product', price: 10.0, status: 'PUBLISHED' } }),
				prismaClient.product.create({ data: { name: 'A Product', price: 20.0, status: 'PUBLISHED' } }),
				prismaClient.product.create({ data: { name: 'M Product', price: 15.0, status: 'PUBLISHED' } }),
			])

			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query SortProducts {
							products(args: {
								sort: { name: ASC }
							}) {
								edges {
									node {
										name
										price
									}
								}
							}
						}
					`,
				}),
			})

			const result = await response.json()
			expect(result.errors).toBeUndefined()

			const products = result.data.products.edges.map((edge: any) => edge.node)
			expect(products[0]?.name).toBe('A Product')
			expect(products[1]?.name).toBe('M Product')
			expect(products[2]?.name).toBe('Z Product')
		})
	})

	describe('Enums', () => {
		test('Product status enum works correctly', async () => {
			await prismaClient.product.create({
				data: { name: 'Test Product', price: 10.0, status: 'ARCHIVED' },
			})

			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query GetArchivedProducts {
							products(args: {
								filter: { status: { equals: ARCHIVED } }
							}) {
								edges {
									node {
										name
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
			expect(result.data.products.edges).toHaveLength(1)
			expect(result.data.products.edges[0]?.node.status).toBe('ARCHIVED')
		})

		test('Review rating enum works correctly', async () => {
			const product = await prismaClient.product.create({
				data: { name: 'Test Product', price: 10.0, status: 'PUBLISHED' },
			})

			await prismaClient.review.create({
				data: {
					title: 'Great product!',
					content: 'Really good',
					rating: 'FIVE',
					productId: product.id,
					verified: true,
				},
			})

			const response = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query GetFiveStarReviews {
							reviews(args: {
								filter: { rating: { equals: FIVE } }
							}) {
								edges {
									node {
										title
										rating
										verified
									}
								}
							}
						}
					`,
				}),
			})

			const result = await response.json()
			expect(result.errors).toBeUndefined()
			expect(result.data.reviews.edges).toHaveLength(1)
			expect(result.data.reviews.edges[0]?.node.rating).toBe('FIVE')
		})
	})

	describe('Relations and Many-to-Many', () => {
		test('Product-Tag many-to-many relationship works', async () => {
			const product = await prismaClient.product.create({
				data: { name: 'Test Product', price: 10.0, status: 'PUBLISHED' },
			})

			const tag = await prismaClient.tag.create({
				data: { name: 'test-tag', color: '#FF0000' },
			})

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

			const queryResponse = await fetch(GRAPHQL_ENDPOINT, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify({
					query: `
						query GetProductWithTags($id: ID!) {
							product(id: $id) {
								name
								tags {
									tag {
										name
										color
									}
								}
							}
						}
					`,
					variables: { id: product.id },
				}),
			})

			const queryResult = await queryResponse.json()
			expect(queryResult.errors).toBeUndefined()
			expect(queryResult.data.product.tags).toHaveLength(1)
			expect(queryResult.data.product.tags[0]?.tag.name).toBe('test-tag')
		})
	})
})
