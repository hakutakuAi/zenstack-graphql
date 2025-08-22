import { expect, test, describe, beforeAll } from 'bun:test'
import { existsSync, readFileSync } from 'fs'
import { join } from 'path'
import { SchemaComposer } from 'graphql-compose'

describe('Basic Example', () => {
	const schemaPath = join(__dirname, './schema.graphql')
	const schemaComposer = new SchemaComposer()

	beforeAll(() => {
		expect(existsSync(schemaPath)).toBe(true)
		schemaComposer.add(readFileSync(schemaPath, 'utf8'))
	})

	test('Schema contains all expected object types', () => {
		const expectedTypes = ['Author', 'Book', 'Review', 'Article', 'Publisher']

		for (const typeName of expectedTypes) {
			expect(schemaComposer.has(typeName)).toBe(true)
			expect(schemaComposer.getOTC(typeName)).toBeTruthy()
		}
	})

	test('Object types have expected fields', () => {
		const authorType = schemaComposer.getOTC('Author')
		expect(authorType.hasField('id')).toBe(true)
		expect(authorType.hasField('name')).toBe(true)
		expect(authorType.hasField('books')).toBe(true)
		expect(authorType.getFieldType('books').toString()).toBe('[Book!]!')

		const bookType = schemaComposer.getOTC('Book')
		expect(bookType.hasField('title')).toBe(true)
		expect(bookType.hasField('category')).toBe(true)
		expect(bookType.hasField('author')).toBe(true)
		expect(bookType.getFieldType('author').toString()).toBe('Author!')

		const reviewType = schemaComposer.getOTC('Review')
		expect(reviewType.hasField('rating')).toBe(true)
		expect(reviewType.hasField('book')).toBe(true)
		expect(reviewType.getFieldType('book').toString()).toBe('Book!')
	})

	test('Enum types are properly defined', () => {
		expect(schemaComposer.has('BookCategory')).toBe(true)
		const categoryEnum = schemaComposer.getETC('BookCategory')

		const enumValues = categoryEnum.getFields()
		expect(Object.keys(enumValues)).toContain('FICTION')
		expect(Object.keys(enumValues)).toContain('NONFICTION')
		expect(Object.keys(enumValues)).toContain('SCIENCE')
	})

	test('Scalar types are properly defined', () => {
		expect(schemaComposer.has('DateTime')).toBe(true)
		expect(schemaComposer.has('Decimal')).toBe(true)
		expect(schemaComposer.getSTC('DateTime')).toBeTruthy()
		expect(schemaComposer.getSTC('Decimal')).toBeTruthy()
	})

	test('Relation fields are properly defined', () => {
		const authorType = schemaComposer.getOTC('Author')
		expect(authorType.getFieldType('books').toString()).toBe('[Book!]!')
		expect(authorType.getFieldType('articles').toString()).toBe('[Article!]!')

		const bookType = schemaComposer.getOTC('Book')
		expect(bookType.getFieldType('author').toString()).toBe('Author!')
		expect(bookType.getFieldType('reviews').toString()).toBe('[Review!]!')

		const articleType = schemaComposer.getOTC('Article')
		expect(articleType.getFieldType('author').toString()).toBe('Author!')
	})

	test('All types and fields in schema are valid', () => {
		const schema = schemaComposer.buildSchema()
		expect(schema).toBeTruthy()
	})

	test('Interface types are properly implemented', () => {
		expect(schemaComposer.has('Node')).toBe(true)
		const nodeInterface = schemaComposer.getIFTC('Node')
		expect(nodeInterface.hasField('id')).toBe(true)
		expect(nodeInterface.getFieldType('id').toString()).toBe('ID!')
	})

	test('Input types for pagination are comprehensive', () => {
		const paginationInputs = ['ForwardPaginationInput', 'BackwardPaginationInput', 'PaginationInput']

		paginationInputs.forEach((inputName) => {
			expect(schemaComposer.has(inputName)).toBe(true)
			const inputType = schemaComposer.getITC(inputName)
			expect(inputType).toBeTruthy()
		})

		const forwardPagination = schemaComposer.getITC('ForwardPaginationInput')
		expect(forwardPagination.hasField('first')).toBe(true)
		expect(forwardPagination.hasField('after')).toBe(true)

		const backwardPagination = schemaComposer.getITC('BackwardPaginationInput')
		expect(backwardPagination.hasField('last')).toBe(true)
		expect(backwardPagination.hasField('before')).toBe(true)
	})

	test('Connection types follow Relay specification', () => {
		const connectionTypes = ['AuthorConnection', 'BookConnection', 'ReviewConnection', 'ArticleConnection', 'PublisherConnection']

		connectionTypes.forEach((connectionName) => {
			expect(schemaComposer.has(connectionName)).toBe(true)
			const connectionType = schemaComposer.getOTC(connectionName)

			expect(connectionType.hasField('pageInfo')).toBe(true)
			expect(connectionType.hasField('edges')).toBe(true)
			expect(connectionType.hasField('totalCount')).toBe(true)

			expect(connectionType.getFieldType('pageInfo').toString()).toBe('PageInfo!')
			expect(connectionType.getFieldType('totalCount').toString()).toBe('Int!')
		})
	})

	test('Edge types follow Relay specification', () => {
		const edgeTypes = ['AuthorEdge', 'BookEdge', 'ReviewEdge', 'ArticleEdge', 'PublisherEdge']

		edgeTypes.forEach((edgeName) => {
			expect(schemaComposer.has(edgeName)).toBe(true)
			const edgeType = schemaComposer.getOTC(edgeName)

			expect(edgeType.hasField('node')).toBe(true)
			expect(edgeType.hasField('cursor')).toBe(true)
			expect(edgeType.getFieldType('cursor').toString()).toBe('String!')
		})
	})

	test('Sort input types have proper enum references', () => {
		const sortInputs = ['AuthorSortInput', 'BookSortInput', 'ReviewSortInput', 'ArticleSortInput', 'PublisherSortInput']

		sortInputs.forEach((sortInputName) => {
			expect(schemaComposer.has(sortInputName)).toBe(true)
			const sortInput = schemaComposer.getITC(sortInputName)
			expect(sortInput.hasField('_placeholder')).toBe(true)
		})

		expect(schemaComposer.has('SortDirection')).toBe(true)
		const sortDirection = schemaComposer.getETC('SortDirection')
		const enumValues = sortDirection.getFields()
		expect(Object.keys(enumValues)).toContain('ASC')
		expect(Object.keys(enumValues)).toContain('DESC')
	})

	test('Filter input types are comprehensive', () => {
		const filterInputs = ['NumericFilterInput', 'DateTimeFilterInput', 'StringFilterInput', 'BooleanFilterInput']

		filterInputs.forEach((filterName) => {
			expect(schemaComposer.has(filterName)).toBe(true)
			const filterType = schemaComposer.getITC(filterName)
			expect(filterType).toBeTruthy()
		})

		const stringFilter = schemaComposer.getITC('StringFilterInput')
		const stringOperations = ['equals', 'not', 'in', 'notIn', 'contains', 'startsWith', 'endsWith']
		stringOperations.forEach((operation) => {
			expect(stringFilter.hasField(operation)).toBe(true)
		})

		const numericFilter = schemaComposer.getITC('NumericFilterInput')
		const numericOperations = ['equals', 'not', 'gt', 'lt']
		numericOperations.forEach((operation) => {
			expect(numericFilter.hasField(operation)).toBe(true)
		})
	})

	test('All model fields have correct GraphQL types', () => {
		const bookType = schemaComposer.getOTC('Book')
		expect(bookType.getFieldType('id').toString()).toBe('String!')
		expect(bookType.getFieldType('title').toString()).toBe('String!')
		expect(bookType.getFieldType('pages').toString()).toBe('Int!')
		expect(bookType.getFieldType('price').toString()).toBe('Decimal!')
		expect(bookType.getFieldType('inStock').toString()).toBe('Boolean!')
		expect(bookType.getFieldType('published').toString()).toBe('DateTime!')
		expect(bookType.getFieldType('category').toString()).toBe('BookCategory!')

		const reviewType = schemaComposer.getOTC('Review')
		expect(reviewType.getFieldType('rating').toString()).toBe('Int!')
		expect(reviewType.getFieldType('text').toString()).toBe('String')
		expect(reviewType.getFieldType('readerName').toString()).toBe('String!')
		expect(reviewType.getFieldType('readerEmail').toString()).toBe('String!')
	})

	test('Bi-directional relationships are properly defined', () => {
		const authorType = schemaComposer.getOTC('Author')
		expect(authorType.getFieldType('books').toString()).toBe('[Book!]!')
		expect(authorType.getFieldType('articles').toString()).toBe('[Article!]!')

		const bookType = schemaComposer.getOTC('Book')
		expect(bookType.getFieldType('author').toString()).toBe('Author!')
		expect(bookType.getFieldType('reviews').toString()).toBe('[Review!]!')

		const reviewType = schemaComposer.getOTC('Review')
		expect(reviewType.getFieldType('book').toString()).toBe('Book!')

		const articleType = schemaComposer.getOTC('Article')
		expect(articleType.getFieldType('author').toString()).toBe('Author!')
	})

	test('Schema descriptions are properly set', () => {
		const nodeInterface = schemaComposer.getIFTC('Node')
		expect(nodeInterface.getDescription()).toContain('unique identifier')

		const dateTimeScalar = schemaComposer.getSTC('DateTime')
		expect(dateTimeScalar.getDescription()).toContain('date-time string')

		const decimalScalar = schemaComposer.getSTC('Decimal')
		expect(decimalScalar.getDescription()).toContain('arbitrary-precision Decimal')
	})
})
