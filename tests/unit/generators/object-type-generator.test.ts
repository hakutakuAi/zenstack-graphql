import { describe, test, expect, beforeEach } from 'bun:test'
import { UnifiedObjectTypeGenerator } from '@generators/unified/unified-object-type-generator'
import { TestFixtures, TestMockFactory, SpyOutputStrategy } from '../../helpers'

describe('UnifiedObjectTypeGenerator', () => {
	let generator: UnifiedObjectTypeGenerator
	let context: any
	let spyStrategy: SpyOutputStrategy

	beforeEach(() => {
		const baseContext = TestFixtures.createContext({
			includeRelations: true,
			models: [
				TestFixtures.createDataModel('User', [
					TestFixtures.createField('id', 'String'),
					TestFixtures.createField('name', 'String'),
					TestFixtures.createField('email', 'String'),
					TestFixtures.createField('age', 'Int'),
					TestFixtures.createField('isActive', 'Boolean'),
					TestFixtures.createField('createdAt', 'DateTime'),
					TestFixtures.createRelationField('posts', 'Post', false, true),
				]),
				TestFixtures.createDataModel('Post', [
					TestFixtures.createField('id', 'String'),
					TestFixtures.createField('title', 'String'),
					TestFixtures.createField('content', 'String', true),
					TestFixtures.createField('published', 'Boolean'),
					TestFixtures.createField('viewCount', 'Int'),
					TestFixtures.createRelationField('author', 'User'),
				]),
			],
		})

		const spyContext = TestMockFactory.createSpyUnifiedContext(baseContext)
		context = spyContext
		spyStrategy = spyContext.spy
		generator = new UnifiedObjectTypeGenerator(context)
	})

	describe('Initialization', () => {
		test('should initialize successfully', () => {
			expect(generator).toBeDefined()
		})
	})

	describe('Object Type Generation', () => {
		test('should generate object types for models', () => {
			const result = generator.generate()

			expect(result).toBeDefined()
			expect(Array.isArray(result)).toBe(true)
			expect(result.length).toBeGreaterThan(0)
		})

		test('should generate correct number of object types', () => {
			const result = generator.generate()

			expect(result.length).toBe(2)
			expect(result).toContain('User')
			expect(result).toContain('Post')
		})

		test('should create object types with fields', () => {
			generator.generate()

			const objectCalls = spyStrategy.getCallsForMethod('createObjectType')
			expect(objectCalls.length).toBe(2)

			const userObjectCall = objectCalls.find((call) => call.args[0] === 'User')
			expect(userObjectCall).toBeDefined()

			const userFields = userObjectCall!.args[1]
			expect(Object.keys(userFields).length).toBeGreaterThan(0)
			expect(userFields).toHaveProperty('id')
			expect(userFields).toHaveProperty('name')
			expect(userFields).toHaveProperty('email')
		})

		test('should handle type existence checks', () => {
			const result = generator.generate()

			expect(result.length).toBe(2)
			expect(result).toContain('User')
			expect(result).toContain('Post')
		})
	})

	describe('Field Processing', () => {
		test('should handle scalar fields correctly', () => {
			generator.generate()

			const objectCalls = spyStrategy.getCallsForMethod('createObjectType')
			const userObjectCall = objectCalls.find((call) => call.args[0] === 'User')

			const fields = userObjectCall!.args[1]
			expect(fields.id).toBeDefined()
			expect(fields.id.type).toBe('String!')
			expect(fields.name).toBeDefined()
			expect(fields.name.type).toBe('String!')
			expect(fields.age).toBeDefined()
			expect(fields.age.type).toBe('Int!')
			expect(fields.isActive).toBeDefined()
			expect(fields.isActive.type).toBe('Boolean!')
		})

		test('should handle optional fields correctly', () => {
			generator.generate()

			const objectCalls = spyStrategy.getCallsForMethod('createObjectType')
			const postObjectCall = objectCalls.find((call) => call.args[0] === 'Post')

			const fields = postObjectCall!.args[1]
			expect(fields.content).toBeDefined()
			expect(fields.content.type).toBe('String')
		})

		test('should include relation fields when enabled', () => {
			generator.generate()

			const objectCalls = spyStrategy.getCallsForMethod('createObjectType')
			const userObjectCall = objectCalls.find((call) => call.args[0] === 'User')

			const fields = userObjectCall!.args[1]
			expect(fields.posts).toBeDefined()
		})

		test('should exclude relation fields when disabled', () => {
			const noRelationContext = TestFixtures.createContext({
				includeRelations: false,
				models: [
					TestFixtures.createDataModel('User', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('name', 'String'),
						TestFixtures.createRelationField('posts', 'Post', false, true),
					]),
				],
			})

			const noRelationUnifiedContext = TestMockFactory.createSpyUnifiedContext(noRelationContext)
			const noRelationGenerator = new UnifiedObjectTypeGenerator(noRelationUnifiedContext)

			noRelationGenerator.generate()

			const objectCalls = noRelationUnifiedContext.spy.getCallsForMethod('createObjectType')
			const userObjectCall = objectCalls.find((call) => call.args[0] === 'User')

			const fields = userObjectCall!.args[1]
			expect(fields.posts).toBeUndefined()
			expect(fields.id).toBeDefined()
			expect(fields.name).toBeDefined()
		})

		test('should create field descriptions', () => {
			generator.generate()

			const objectCalls = spyStrategy.getCallsForMethod('createObjectType')
			const userObjectCall = objectCalls.find((call) => call.args[0] === 'User')

			const fields = userObjectCall!.args[1]
			Object.values(fields).forEach((field: any) => {
				expect(field).toHaveProperty('type')
				expect(field.type).toBeDefined()
			})
		})
	})

	describe('Type Naming', () => {
		test('should format object type names correctly', () => {
			const result = generator.generate()

			expect(result).toContain('User')
			expect(result).toContain('Post')
		})

		test('should handle custom naming conventions', () => {
			const customContext = TestFixtures.createContext({
				typeNaming: 'camelCase',
				fieldNaming: 'snake_case',
				models: [TestFixtures.createDataModel('user_profile', [TestFixtures.createField('first_name', 'String')])],
			})

			const customUnifiedContext = TestMockFactory.createSpyUnifiedContext(customContext)
			const customGenerator = new UnifiedObjectTypeGenerator(customUnifiedContext)

			const result = customGenerator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBe(1)
		})
	})

	describe('Error Handling', () => {
		test('should handle empty models gracefully', () => {
			const emptyContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
					models: [],
				}),
			)
			const emptyGenerator = new UnifiedObjectTypeGenerator(emptyContext)

			const result = emptyGenerator.generate()
			expect(result).toBeDefined()
			expect(result.length).toBe(0)
		})

		test('should handle models with no fields gracefully', () => {
			const noFieldsContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
					models: [TestFixtures.createDataModel('Empty', [])],
				}),
			)
			const noFieldsGenerator = new UnifiedObjectTypeGenerator(noFieldsContext)

			expect(() => {
				const result = noFieldsGenerator.generate()
				expect(result).toBeDefined()
			}).not.toThrow()
		})

		test('should handle field type mapping errors gracefully', () => {
			const malformedContext = TestFixtures.createContext({
				models: [
					TestFixtures.createDataModel('Test', [
						{
							$type: 'DataModelField',
							name: 'invalidField',
							type: {
								type: undefined,
								optional: false,
								array: false,
								reference: null,
							},
							attributes: [TestFixtures.createAttribute('@graphql.filterable')],
						},
					]),
				],
			})

			const malformedUnifiedContext = TestMockFactory.createUnifiedContext(malformedContext)
			const malformedGenerator = new UnifiedObjectTypeGenerator(malformedUnifiedContext)

			expect(() => {
				const result = malformedGenerator.generate()
				expect(result).toBeDefined()
			}).not.toThrow()
		})
	})

	describe('Result Processing', () => {
		test('should return only object type names', () => {
			const result = generator.generate()

			result.forEach((typeName) => {
				expect(typeof typeName).toBe('string')
				expect(typeName.length).toBeGreaterThan(0)
			})
		})

		test('should validate actual object structure', () => {
			const result = generator.generate()

			expect(result.length).toBe(2)
			expect(result).toContain('User')
			expect(result).toContain('Post')

			const objectCalls = spyStrategy.getCallsForMethod('createObjectType')
			expect(objectCalls.length).toBe(2)

			const userObjectCall = objectCalls.find((call) => call.args[0] === 'User')
			expect(userObjectCall).toBeDefined()
			expect(Object.keys(userObjectCall!.args[1]).length).toBeGreaterThan(0)
		})
	})

	describe('Complex Scenarios', () => {
		test('should handle models with mixed field types', () => {
			const mixedContext = TestFixtures.createContext({
				includeRelations: true,
				models: [
					TestFixtures.createDataModel('ComplexModel', [
						TestFixtures.createField('stringField', 'String'),
						TestFixtures.createField('intField', 'Int'),
						TestFixtures.createField('floatField', 'Float'),
						TestFixtures.createField('boolField', 'Boolean'),
						TestFixtures.createField('dateField', 'DateTime'),
						TestFixtures.createField('optionalField', 'String', true),
						TestFixtures.createRelationField('relation', 'OtherModel'),
					]),
				],
			})

			const mixedUnifiedContext = TestMockFactory.createSpyUnifiedContext(mixedContext)
			const mixedGenerator = new UnifiedObjectTypeGenerator(mixedUnifiedContext)

			const result = mixedGenerator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBe(1)
			expect(result).toContain('ComplexModel')

			const objectCalls = mixedUnifiedContext.spy.getCallsForMethod('createObjectType')
			expect(objectCalls.length).toBe(1)

			const complexObjectCall = objectCalls[0]
			const fields = complexObjectCall!.args[1]
			expect(Object.keys(fields).length).toBeGreaterThan(0)
		})

		test('should handle single model correctly', () => {
			const singleContext = TestFixtures.createContext({
				models: [TestFixtures.createDataModel('Single', [TestFixtures.createField('name', 'String')])],
			})

			const singleUnifiedContext = TestMockFactory.createUnifiedContext(singleContext)
			const singleGenerator = new UnifiedObjectTypeGenerator(singleUnifiedContext)

			const result = singleGenerator.generate()
			expect(result).toBeDefined()
			expect(result.length).toBe(1)
			expect(result).toContain('Single')
		})

		test('should handle deeply nested type scenarios', () => {
			const nestedContext = TestFixtures.createContext({
				includeRelations: true,
				models: [
					TestFixtures.createDataModel('Organization', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('name', 'String'),
						TestFixtures.createRelationField('users', 'User', false, true),
					]),
					TestFixtures.createDataModel('User', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('name', 'String'),
						TestFixtures.createRelationField('organization', 'Organization'),
						TestFixtures.createRelationField('posts', 'Post', false, true),
					]),
					TestFixtures.createDataModel('Post', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('title', 'String'),
						TestFixtures.createRelationField('author', 'User'),
					]),
				],
			})

			const nestedUnifiedContext = TestMockFactory.createUnifiedContext(nestedContext)
			const nestedGenerator = new UnifiedObjectTypeGenerator(nestedUnifiedContext)

			const result = nestedGenerator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBe(3)
			expect(result).toContain('Organization')
			expect(result).toContain('User')
			expect(result).toContain('Post')
		})
	})

	describe('Field Type Mapping', () => {
		test('should correctly map scalar field types', () => {
			generator.generate()

			const objectCalls = spyStrategy.getCallsForMethod('createObjectType')
			const userObjectCall = objectCalls.find((call) => call.args[0] === 'User')

			const fields = userObjectCall!.args[1]
			expect(fields.id.type).toBe('String!')
			expect(fields.age.type).toBe('Int!')
			expect(fields.isActive.type).toBe('Boolean!')
		})

		test('should correctly map relation field types', () => {
			generator.generate()

			const objectCalls = spyStrategy.getCallsForMethod('createObjectType')
			const userObjectCall = objectCalls.find((call) => call.args[0] === 'User')

			const fields = userObjectCall!.args[1]
			expect(fields.posts).toBeDefined()
		})
	})
})
