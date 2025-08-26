import { describe, it, expect, beforeEach } from 'bun:test'
import { UnifiedInputGenerator } from '@generators/unified/unified-input-generator'
import { TestFixtures, TestMockFactory, SpyOutputStrategy } from '../../helpers'

describe('UnifiedInputGenerator', () => {
	let generator: UnifiedInputGenerator
	let context: any
	let spyStrategy: SpyOutputStrategy

	beforeEach(() => {
		const baseContext = TestFixtures.createContext({
			models: [
				TestFixtures.createDataModel('User', [
					TestFixtures.createField('id', 'String'),
					TestFixtures.createField('name', 'String'),
					TestFixtures.createField('email', 'String'),
					TestFixtures.createField('age', 'Int'),
					TestFixtures.createField('isActive', 'Boolean'),
					TestFixtures.createField('createdAt', 'DateTime'),
				]),
				TestFixtures.createDataModel('Post', [
					TestFixtures.createField('id', 'String'),
					TestFixtures.createField('title', 'String'),
					TestFixtures.createField('content', 'String', true),
					TestFixtures.createField('published', 'Boolean'),
					TestFixtures.createField('viewCount', 'Int'),
				]),
			],
		})

		const spyContext = TestMockFactory.createSpyUnifiedContext(baseContext)
		context = spyContext
		spyStrategy = spyContext.spy
		generator = new UnifiedInputGenerator(context)
	})

	describe('Initialization', () => {
		it('should initialize successfully', () => {
			expect(generator).toBeDefined()
		})
	})

	describe('Input Generation', () => {
		it('should generate input types for models', () => {
			const result = generator.generate()

			expect(result).toBeDefined()
			expect(Array.isArray(result)).toBe(true)
			expect(result.length).toBeGreaterThan(0)
		})

		it('should generate both create and update inputs', () => {
			generator.generate()

			const inputCalls = spyStrategy.getCallsForMethod('createInputType')
			expect(inputCalls.length).toBeGreaterThanOrEqual(4)

			const createInputs = inputCalls.filter((call) => call.args[2] === 'create')
			const updateInputs = inputCalls.filter((call) => call.args[2] === 'update')

			expect(createInputs.length).toBe(2)
			expect(updateInputs.length).toBe(2)
		})

		it('should generate correct input type names', () => {
			generator.generate()

			const inputCalls = spyStrategy.getCallsForMethod('createInputType')
			const typeNames = inputCalls.map((call) => call.args[0])

			expect(typeNames).toContain('UserCreateInput')
			expect(typeNames).toContain('UserUpdateInput')
			expect(typeNames).toContain('PostCreateInput')
			expect(typeNames).toContain('PostUpdateInput')
		})

		it('should include descriptions for input types', () => {
			generator.generate()

			const inputCalls = spyStrategy.getCallsForMethod('createInputType')

			inputCalls.forEach((call) => {
				const description = call.args[3]
				expect(description).toBeDefined()
				expect(typeof description).toBe('string')
				expect(description.length).toBeGreaterThan(0)
			})
		})
	})

	describe('Create Input Generation', () => {
		it('should generate create input with correct parameters', () => {
			generator.generate()

			const createCalls = spyStrategy.getCallsForMethod('createInputType').filter((call) => call.args[2] === 'create')
			expect(createCalls.length).toBe(2)

			const userCreateCall = createCalls.find((call) => call.args[0] === 'UserCreateInput')
			expect(userCreateCall).toBeDefined()
			expect(userCreateCall!.args[1].name).toBe('User')
			expect(userCreateCall!.args[2]).toBe('create')
			expect(userCreateCall!.args[3]).toContain('Create input for User')
		})

		it('should handle models with optional fields for create input', () => {
			const optionalFieldContext = TestFixtures.createContext({
				models: [
					TestFixtures.createDataModel('Product', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('name', 'String'),
						TestFixtures.createField('description', 'String', true),
						TestFixtures.createField('price', 'Float'),
					]),
				],
			})

			const optionalUnifiedContext = TestMockFactory.createSpyUnifiedContext(optionalFieldContext)
			const optionalGenerator = new UnifiedInputGenerator(optionalUnifiedContext)

			optionalGenerator.generate()

			const createCalls = optionalUnifiedContext.spy.getCallsForMethod('createInputType').filter((call) => call.args[2] === 'create')
			expect(createCalls.length).toBe(1)

			const productCreateCall = createCalls[0]
			expect(productCreateCall!.args[0]).toBe('ProductCreateInput')
			expect(productCreateCall!.args[1].name).toBe('Product')
		})
	})

	describe('Update Input Generation', () => {
		it('should generate update input with correct parameters', () => {
			generator.generate()

			const updateCalls = spyStrategy.getCallsForMethod('createInputType').filter((call) => call.args[2] === 'update')
			expect(updateCalls.length).toBe(2)

			const userUpdateCall = updateCalls.find((call) => call.args[0] === 'UserUpdateInput')
			expect(userUpdateCall).toBeDefined()
			expect(userUpdateCall!.args[1].name).toBe('User')
			expect(userUpdateCall!.args[2]).toBe('update')
			expect(userUpdateCall!.args[3]).toContain('Update input for User')
		})

		it('should handle models with different field types for update input', () => {
			generator.generate()

			const updateCalls = spyStrategy.getCallsForMethod('createInputType').filter((call) => call.args[2] === 'update')
			const postUpdateCall = updateCalls.find((call) => call.args[0] === 'PostUpdateInput')

			expect(postUpdateCall).toBeDefined()
			expect(postUpdateCall!.args[1].fields.length).toBeGreaterThan(0)
		})
	})

	describe('Error Handling', () => {
		it('should handle empty models gracefully', () => {
			const emptyContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
					models: [],
				}),
			)
			const emptyGenerator = new UnifiedInputGenerator(emptyContext)

			const result = emptyGenerator.generate()
			expect(result).toBeDefined()
			expect(result.length).toBe(0)
		})

		it('should handle models with no fields gracefully', () => {
			const noFieldsContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
					models: [TestFixtures.createDataModel('Empty', [])],
				}),
			)
			const noFieldsGenerator = new UnifiedInputGenerator(noFieldsContext)

			expect(() => {
				const result = noFieldsGenerator.generate()
				expect(result).toBeDefined()
			}).not.toThrow()
		})

		it('should handle generation errors gracefully', () => {
			const malformedModel = {
				$type: 'DataModel',
				name: null,
				fields: [],
				attributes: [],
				isAbstract: false,
				$container: undefined,
				comments: [],
				isView: false,
				superTypes: [],
			}

			const errorContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
					models: [malformedModel as any],
				}),
			)
			const errorGenerator = new UnifiedInputGenerator(errorContext)

			expect(() => {
				const result = errorGenerator.generate()
				expect(result).toBeDefined()
			}).not.toThrow()
		})
	})

	describe('Type Naming', () => {
		it('should format input type names correctly', () => {
			generator.generate()

			const inputCalls = spyStrategy.getCallsForMethod('createInputType')
			const typeNames = inputCalls.map((call) => call.args[0])

			typeNames.forEach((typeName) => {
				expect(typeName).toMatch(/^[A-Z][a-zA-Z0-9]*(Create|Update)Input$/)
			})
		})

		it('should handle custom naming conventions', () => {
			const customContext = TestFixtures.createContext({
				typeNaming: 'camelCase',
				fieldNaming: 'snake_case',
				models: [TestFixtures.createDataModel('user_profile', [TestFixtures.createField('first_name', 'String')])],
			})

			const customUnifiedContext = TestMockFactory.createSpyUnifiedContext(customContext)
			const customGenerator = new UnifiedInputGenerator(customUnifiedContext)

			customGenerator.generate()

			const inputCalls = customUnifiedContext.spy.getCallsForMethod('createInputType')
			expect(inputCalls.length).toBe(2)

			const typeNames = inputCalls.map((call) => call.args[0])
			expect(typeNames).toContain('userProfileCreateInput')
			expect(typeNames).toContain('userProfileUpdateInput')
		})
	})

	describe('Result Processing', () => {
		it('should return consistent result structure', () => {
			const result = generator.generate()

			expect(Array.isArray(result)).toBe(true)
			result.forEach((item) => {
				expect(typeof item).toBe('string')
				expect(item.length).toBeGreaterThan(0)
			})
		})

		it('should validate actual input structure', () => {
			const result = generator.generate()

			expect(result.length).toBe(2)

			const inputCalls = spyStrategy.getCallsForMethod('createInputType')
			expect(inputCalls.length).toBe(4)

			const userCalls = inputCalls.filter((call) => call.args[0].startsWith('User'))
			const postCalls = inputCalls.filter((call) => call.args[0].startsWith('Post'))

			expect(userCalls.length).toBe(2)
			expect(postCalls.length).toBe(2)
		})
	})

	describe('Complex Scenarios', () => {
		it('should handle models with relations', () => {
			const relationContext = TestFixtures.createContext({
				includeRelations: true,
				models: [
					TestFixtures.createDataModel('User', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('name', 'String'),
						TestFixtures.createRelationField('posts', 'Post', false, true),
					]),
					TestFixtures.createDataModel('Post', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('title', 'String'),
						TestFixtures.createRelationField('author', 'User'),
					]),
				],
			})

			const relationUnifiedContext = TestMockFactory.createSpyUnifiedContext(relationContext)
			const relationGenerator = new UnifiedInputGenerator(relationUnifiedContext)

			const result = relationGenerator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBe(2)

			const inputCalls = relationUnifiedContext.spy.getCallsForMethod('createInputType')
			expect(inputCalls.length).toBe(4)
		})

		it('should handle single model correctly', () => {
			const singleContext = TestFixtures.createContext({
				models: [TestFixtures.createDataModel('Single', [TestFixtures.createField('name', 'String')])],
			})

			const singleUnifiedContext = TestMockFactory.createUnifiedContext(singleContext)
			const singleGenerator = new UnifiedInputGenerator(singleUnifiedContext)

			const result = singleGenerator.generate()
			expect(result).toBeDefined()
			expect(result.length).toBe(1)
		})

		it('should handle models with mixed field types', () => {
			const mixedContext = TestFixtures.createContext({
				models: [
					TestFixtures.createDataModel('ComplexModel', [
						TestFixtures.createField('stringField', 'String'),
						TestFixtures.createField('intField', 'Int'),
						TestFixtures.createField('floatField', 'Float'),
						TestFixtures.createField('boolField', 'Boolean'),
						TestFixtures.createField('dateField', 'DateTime'),
						TestFixtures.createField('optionalField', 'String', true),
					]),
				],
			})

			const mixedUnifiedContext = TestMockFactory.createSpyUnifiedContext(mixedContext)
			const mixedGenerator = new UnifiedInputGenerator(mixedUnifiedContext)

			const result = mixedGenerator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBe(1)

			const inputCalls = mixedUnifiedContext.spy.getCallsForMethod('createInputType')
			expect(inputCalls.length).toBe(2)

			const createCall = inputCalls.find((call) => call.args[0] === 'ComplexModelCreateInput')
			expect(createCall).toBeDefined()
			expect(createCall!.args[1].fields.length).toBe(6)
		})
	})
})
