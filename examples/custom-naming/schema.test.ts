import { expect, test, describe, beforeAll } from 'bun:test'
import { existsSync, readFileSync } from 'fs'
import { join } from 'path'
import { SchemaComposer } from 'graphql-compose'

describe('Custom Naming Example', () => {
	const schemaPath = join(__dirname, './schema.graphql')
	const schemaComposer = new SchemaComposer()

	beforeAll(() => {
		expect(existsSync(schemaPath)).toBe(true)
		schemaComposer.add(readFileSync(schemaPath, 'utf8'))
	})

	test('Field names use snake_case convention', () => {
		const customerTC = schemaComposer.getOTC('Customer')

		expect(customerTC.hasField('first_name')).toBe(true)
		expect(customerTC.hasField('last_name')).toBe(true)
		expect(customerTC.hasField('email_address')).toBe(true)
		expect(customerTC.hasField('registration_date')).toBe(true)
		expect(customerTC.hasField('loyalty_points')).toBe(true)

		expect(customerTC.hasField('firstName')).toBe(false)
		expect(customerTC.hasField('lastName')).toBe(false)
		expect(customerTC.hasField('emailAddress')).toBe(false)

		const productTC = schemaComposer.getOTC('Product')
		expect(productTC.hasField('stock_quantity')).toBe(true)
		expect(productTC.hasField('stockQuantity')).toBe(false)
	})

	test('Type names use PascalCase convention', () => {
		expect(schemaComposer.has('Customer')).toBe(true)
		expect(schemaComposer.has('Product')).toBe(true)
		expect(schemaComposer.has('Address')).toBe(true)
		expect(schemaComposer.has('Order')).toBe(true)
		expect(schemaComposer.has('OrderItem')).toBe(true)
		expect(schemaComposer.has('Category')).toBe(true)
	})

	test('Custom field naming overrides work correctly', () => {
		const customerTC = schemaComposer.getOTC('Customer')
		expect(customerTC.hasField('contact_number')).toBe(true)
		expect(customerTC.hasField('phoneNumber')).toBe(false)

		const orderItemTC = schemaComposer.getOTC('OrderItem')
		expect(orderItemTC.hasField('discount_amount')).toBe(true)
		expect(orderItemTC.hasField('itemDiscount')).toBe(false)
	})

	test('Custom model naming works correctly', () => {
		expect(schemaComposer.has('Product')).toBe(true)
		expect(schemaComposer.has('ProductItem')).toBe(false)
	})

	test('Relations follow the naming conventions', () => {
		const customerTC = schemaComposer.getOTC('Customer')
		expect(customerTC.hasField('orders')).toBe(true)
		expect(customerTC.hasField('shipping_address')).toBe(true)
		expect(customerTC.hasField('billing_address')).toBe(true)
		expect(customerTC.getFieldType('shipping_address').toString()).toBe('Address')

		const productTC = schemaComposer.getOTC('Product')
		expect(productTC.hasField('order_items')).toBe(true)
		expect(productTC.hasField('categories')).toBe(true)
	})

	test('Custom descriptions are properly set', () => {
		const addressTC = schemaComposer.getOTC('Address')
		expect(addressTC.getDescription()).toBe('Shipping or billing address information')

		const orderTC = schemaComposer.getOTC('Order')
		const instructionsField = orderTC.getField('special_instructions')
		expect(instructionsField.description).toBe('Special delivery instructions')
	})

	test('Connection types follow naming conventions', () => {
		expect(schemaComposer.has('CustomerConnection')).toBe(true)
		expect(schemaComposer.has('ProductConnection')).toBe(true)
		expect(schemaComposer.has('AddressConnection')).toBe(true)

		const customerConnectionTC = schemaComposer.getOTC('CustomerConnection')
		expect(customerConnectionTC.hasField('edges')).toBe(true)
		expect(customerConnectionTC.hasField('pageInfo')).toBe(true)
		expect(customerConnectionTC.hasField('totalCount')).toBe(true)

		const customerEdgeTC = schemaComposer.getOTC('CustomerEdge')
		expect(customerEdgeTC.hasField('node')).toBe(true)
		expect(customerEdgeTC.hasField('cursor')).toBe(true)
	})

	test('Schema is valid', () => {
		const schema = schemaComposer.buildSchema()
		expect(schema).toBeTruthy()
	})
})
