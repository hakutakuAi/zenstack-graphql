import { expect, test, describe, beforeAll } from 'bun:test'
import { existsSync, readFileSync } from 'fs'
import { join } from 'path'
import { SchemaComposer, ObjectTypeComposer, InputTypeComposer, InterfaceTypeComposer } from 'graphql-compose'

describe('Full Features Example', () => {
	const schemaPath = join(__dirname, '../schema.graphql')
	const schemaComposer = new SchemaComposer()

	beforeAll(() => {
		expect(existsSync(schemaPath)).toBe(true)
		schemaComposer.add(readFileSync(schemaPath, 'utf8'))
	})

	test('Custom model name via @@graphql.name', () => {
		expect(schemaComposer.has('Merchandise')).toBe(true)
		expect(schemaComposer.has('Product')).toBe(false)

		const merchandise = schemaComposer.getOTC('Merchandise')
		expect(merchandise).toBeTruthy()
	})

	test('Custom model description via @@graphql.description', () => {
		const merchandise = schemaComposer.getOTC('Merchandise')
		expect(merchandise.getDescription()).toBe('A product available for purchase')
	})

	test('Model is ignored via @@graphql.ignore', () => {
		expect(schemaComposer.has('InternalConfig')).toBe(false)
	})

	test('Custom field name via @graphql.name', () => {
		const review = schemaComposer.getOTC('Review')
		expect(review.hasField('reviewer')).toBe(true)
		expect(review.hasField('authorName')).toBe(false)

		const profile = schemaComposer.getOTC('Profile')
		expect(profile.hasField('profilePicture')).toBe(true)
		expect(profile.hasField('avatarUrl')).toBe(false)
	})

	test('Field is ignored via @graphql.ignore', () => {
		const profile = schemaComposer.getOTC('Profile')
		expect(profile.hasField('preferences')).toBe(false)
	})

	test('Custom field description via @graphql.description', () => {
		const review = schemaComposer.getOTC('Review')
		const commentField = review.getField('comment')
		expect(commentField.description).toBe('User feedback')
	})

	test('Connection types are generated', () => {
		expect(schemaComposer.has('MerchandiseConnection')).toBe(true)
		expect(schemaComposer.has('MerchandiseEdge')).toBe(true)

		const merchandiseConnection = schemaComposer.getOTC('MerchandiseConnection')
		expect(merchandiseConnection.hasField('edges')).toBe(true)
		expect(merchandiseConnection.hasField('pageInfo')).toBe(true)
		expect(merchandiseConnection.hasField('totalCount')).toBe(true)

		expect(merchandiseConnection.getFieldType('edges').toString()).toBe('[MerchandiseEdge!]!')
		expect(merchandiseConnection.getFieldType('pageInfo').toString()).toBe('PageInfo!')
		expect(merchandiseConnection.getFieldType('totalCount').toString()).toBe('Int!')

		const merchandiseEdge = schemaComposer.getOTC('MerchandiseEdge')
		expect(merchandiseEdge.hasField('node')).toBe(true)
		expect(merchandiseEdge.hasField('cursor')).toBe(true)
		expect(merchandiseEdge.getFieldType('node').toString()).toBe('Merchandise!')
	})

	test('Sort inputs are generated for @graphql.sortable fields', () => {
		expect(schemaComposer.has('MerchandiseSortInput')).toBe(true)
		expect(schemaComposer.has('ReviewSortInput')).toBe(true)
		expect(schemaComposer.has('OrderSortInput')).toBe(true)

		const merchandiseSortInput = schemaComposer.getITC('MerchandiseSortInput')
		expect(merchandiseSortInput.hasField('createdAt')).toBe(true)
		expect(merchandiseSortInput.hasField('price')).toBe(true)
		expect(merchandiseSortInput.getFieldType('createdAt').toString()).toBe('SortDirection')

		const reviewSortInput = schemaComposer.getITC('ReviewSortInput')
		expect(reviewSortInput.hasField('createdAt')).toBe(true)
		expect(reviewSortInput.hasField('rating')).toBe(true)

		const orderSortInput = schemaComposer.getITC('OrderSortInput')
		expect(orderSortInput.hasField('createdAt')).toBe(true)
		expect(orderSortInput.hasField('totalAmount')).toBe(true)
	})

	test('Filter inputs are generated for @graphql.filterable fields', () => {
		expect(schemaComposer.has('MerchandiseFilterInput')).toBe(true)
		expect(schemaComposer.has('CategoryFilterInput')).toBe(true)
		expect(schemaComposer.has('ReviewFilterInput')).toBe(true)
		expect(schemaComposer.has('OrderFilterInput')).toBe(true)

		const merchandiseFilterInput = schemaComposer.getITC('MerchandiseFilterInput')
		expect(merchandiseFilterInput.hasField('createdAt')).toBe(true)
		expect(merchandiseFilterInput.hasField('name')).toBe(true)
		expect(merchandiseFilterInput.getFieldType('createdAt').toString()).toBe('DateTimeFilterInput')
		expect(merchandiseFilterInput.getFieldType('name').toString()).toBe('StringFilterInput')

		expect(merchandiseFilterInput.hasField('AND')).toBe(true)
		expect(merchandiseFilterInput.hasField('OR')).toBe(true)

		const categoryFilterInput = schemaComposer.getITC('CategoryFilterInput')
		expect(categoryFilterInput.hasField('name')).toBe(true)
		expect(categoryFilterInput.getFieldType('name').toString()).toBe('StringFilterInput')

		const reviewFilterInput = schemaComposer.getITC('ReviewFilterInput')
		expect(reviewFilterInput.hasField('rating')).toBe(true)
		expect(reviewFilterInput.getFieldType('rating').toString()).toBe('NumericFilterInput')

		const orderFilterInput = schemaComposer.getITC('OrderFilterInput')
		expect(orderFilterInput.hasField('status')).toBe(true)
		expect(orderFilterInput.getFieldType('status').toString()).toBe('StringFilterInput')
	})

	test('Relay compliance', () => {
		expect(schemaComposer.has('Node')).toBe(true)
		const nodeInterface = schemaComposer.getIFTC('Node')
		expect(nodeInterface).toBeTruthy()
		expect(nodeInterface.hasField('id')).toBe(true)
		expect(nodeInterface.getFieldType('id').toString()).toBe('ID!')
	})

	test('Basic filter input types are properly defined', () => {
		const numericFilterInput = schemaComposer.getITC('NumericFilterInput')
		expect(numericFilterInput.hasField('equals')).toBe(true)
		expect(numericFilterInput.hasField('not')).toBe(true)
		expect(numericFilterInput.hasField('gt')).toBe(true)
		expect(numericFilterInput.hasField('lt')).toBe(true)

		const dateTimeFilterInput = schemaComposer.getITC('DateTimeFilterInput')
		expect(dateTimeFilterInput.hasField('equals')).toBe(true)
		expect(dateTimeFilterInput.hasField('not')).toBe(true)
		expect(dateTimeFilterInput.hasField('gt')).toBe(true)
		expect(dateTimeFilterInput.hasField('lt')).toBe(true)

		const stringFilterInput = schemaComposer.getITC('StringFilterInput')
		expect(stringFilterInput.hasField('equals')).toBe(true)
		expect(stringFilterInput.hasField('contains')).toBe(true)
		expect(stringFilterInput.hasField('startsWith')).toBe(true)
		expect(stringFilterInput.hasField('endsWith')).toBe(true)

		const booleanFilterInput = schemaComposer.getITC('BooleanFilterInput')
		expect(booleanFilterInput.hasField('equals')).toBe(true)
		expect(booleanFilterInput.hasField('not')).toBe(true)
	})

	test('Schema is valid', () => {
		const schema = schemaComposer.buildSchema()
		expect(schema).toBeTruthy()
	})
})
