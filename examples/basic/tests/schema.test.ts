import { expect, test, describe, beforeAll } from 'bun:test'
import { existsSync, readFileSync } from 'fs'
import { join } from 'path'
import { SchemaComposer, ObjectTypeComposer, EnumTypeComposer } from 'graphql-compose'

describe('Basic Example', () => {
	const schemaPath = join(__dirname, '../schema.graphql')
	const schemaComposer = new SchemaComposer()

	beforeAll(() => {
		expect(existsSync(schemaPath)).toBe(true)
		schemaComposer.add(readFileSync(schemaPath, 'utf8'))
	})

	test('Schema contains all expected object types', () => {
		const expectedTypes = ['Space', 'SpaceUser', 'User', 'List', 'Todo', 'ActivityLog', 'Tag']

		for (const typeName of expectedTypes) {
			expect(schemaComposer.has(typeName)).toBe(true)
			expect(schemaComposer.getOTC(typeName)).toBeTruthy()
		}
	})

	test('Object types have expected fields', () => {
		const spaceType = schemaComposer.getOTC('Space')
		expect(spaceType.hasField('id')).toBe(true)
		expect(spaceType.hasField('name')).toBe(true)
		expect(spaceType.hasField('slug')).toBe(true)
		expect(spaceType.hasField('members')).toBe(true)
		expect(spaceType.hasField('lists')).toBe(true)

		expect(spaceType.getFieldType('id').toString()).toBe('String!')
		expect(spaceType.getFieldType('members').toString()).toBe('[SpaceUser!]!')

		const userType = schemaComposer.getOTC('User')
		expect(userType.hasField('email')).toBe(true)
		expect(userType.hasField('spaces')).toBe(true)
		expect(userType.hasField('lists')).toBe(true)
		expect(userType.hasField('todos')).toBe(true)
		expect(userType.getFieldType('email').toString()).toBe('String!')

		const todoType = schemaComposer.getOTC('Todo')
		expect(todoType.hasField('title')).toBe(true)
		expect(todoType.hasField('priority')).toBe(true)
		expect(todoType.hasField('owner')).toBe(true)
		expect(todoType.getFieldType('title').toString()).toBe('String!')
		expect(todoType.getFieldType('owner').toString()).toBe('User!')
	})

	test('Enum types are properly defined', () => {
		expect(schemaComposer.has('SpaceUserRole')).toBe(true)
		const roleEnum = schemaComposer.getETC('SpaceUserRole')

		const enumValues = roleEnum.getFields()
		expect(Object.keys(enumValues)).toContain('USER')
		expect(Object.keys(enumValues)).toContain('ADMIN')
		expect(Object.keys(enumValues)).toContain('GUEST')
	})

	test('Scalar types are properly defined', () => {
		expect(schemaComposer.has('DateTime')).toBe(true)
		expect(schemaComposer.has('JSON')).toBe(true)
		expect(schemaComposer.getSTC('DateTime')).toBeTruthy()
		expect(schemaComposer.getSTC('JSON')).toBeTruthy()
	})

	test('Relation fields are properly defined', () => {
		const spaceType = schemaComposer.getOTC('Space')
		expect(spaceType.getFieldType('members').toString()).toBe('[SpaceUser!]!')

		const spaceUserType = schemaComposer.getOTC('SpaceUser')
		expect(spaceUserType.getFieldType('space').toString()).toBe('Space!')

		const listType = schemaComposer.getOTC('List')
		expect(listType.getFieldType('todos').toString()).toBe('[Todo!]!')

		const todoType = schemaComposer.getOTC('Todo')
		expect(todoType.getFieldType('list').toString()).toBe('List!')
	})

	test('All types and fields in schema are valid', () => {
		const schema = schemaComposer.buildSchema()
		expect(schema).toBeTruthy()
	})
})
