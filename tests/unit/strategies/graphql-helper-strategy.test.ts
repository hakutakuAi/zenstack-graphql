import { describe, it, expect, beforeEach } from 'bun:test'
import { GraphQLHelperStrategy } from '@generators/strategies/graphql-helper-strategy'
import { TypeScriptHelperStrategy } from '@generators/strategies/typescript-helper-strategy'
import { ModelHelper, HelperGenerationContext } from '@generators/unified/unified-helper-generator'
import { OutputFormat } from '@utils/constants'
import { TestFixtures, TestMockFactory } from '../../helpers'

describe('GraphQLHelperStrategy', () => {
	let strategy: GraphQLHelperStrategy
	let mockContext: HelperGenerationContext

	beforeEach(() => {
		strategy = new GraphQLHelperStrategy()
		mockContext = {
			models: [
				TestFixtures.createDataModel('User', [
					TestFixtures.createField('id', 'String'),
					TestFixtures.createField('name', 'String'),
					TestFixtures.createField('email', 'String'),
				]),
				TestFixtures.createDataModel('Post', [
					TestFixtures.createField('id', 'String'),
					TestFixtures.createField('title', 'String'),
					TestFixtures.createRelationField('author', 'User'),
				]),
			],
			relations: [
				{
					modelName: 'User',
					targetModelName: 'Post',
					fieldName: 'posts',
					targetFieldName: 'author',
					isList: true,
					isRequired: false,
				},
				{
					modelName: 'Post',
					targetModelName: 'User',
					fieldName: 'author',
					targetFieldName: 'posts',
					isList: false,
					isRequired: false,
				},
			],
			outputFormat: OutputFormat.GRAPHQL,
			attributeProcessor: TestMockFactory.createSchemaProcessor(),
		}
	})

	describe('Initialization', () => {
		it('should initialize GraphQLHelperStrategy', () => {
			expect(strategy).toBeDefined()
			expect(strategy).toBeInstanceOf(GraphQLHelperStrategy)
		})

		it('should extend TypeScriptHelperStrategy', () => {
			expect(strategy).toBeInstanceOf(TypeScriptHelperStrategy)
		})

		it('should have generateHelpers method', () => {
			expect(typeof strategy.generateHelpers).toBe('function')
		})
	})

	describe('Helper Generation', () => {
		it('should generate helpers by delegating to parent class', () => {
			const helpers: ModelHelper[] = [
				{
					modelName: 'User',
					connectionBuilderName: 'UserConnectionBuilder',
					filterBuilderName: 'UserFilterBuilder',
					sortBuilderName: 'UserSortBuilder',
					fieldSelectionName: 'UserFieldSelection',
					includesConstName: 'USER_INCLUDES',
					relations: [
						{
							modelName: 'User',
							fieldName: 'posts',
							targetModelName: 'Post',
							targetFieldName: 'author',
							isList: true,
							isRequired: false,
						},
					],
				},
			]

			const result = strategy.generateHelpers(helpers, mockContext)

			expect(result).toBeDefined()
			expect(Array.isArray(result)).toBe(true)
			expect(result.length).toBeGreaterThan(0)
		})

		it('should generate TypeScript helpers even for GraphQL output format', () => {
			const helpers: ModelHelper[] = [
				{
					modelName: 'User',
					connectionBuilderName: 'UserConnectionBuilder',
					filterBuilderName: 'UserFilterBuilder',
					sortBuilderName: 'UserSortBuilder',
					fieldSelectionName: 'UserFieldSelection',
					includesConstName: 'USER_INCLUDES',
					relations: [],
				},
			]

			mockContext.outputFormat = OutputFormat.GRAPHQL

			const result = strategy.generateHelpers(helpers, mockContext)

			expect(result).toBeDefined()
			expect(Array.isArray(result)).toBe(true)
			expect(result.length).toBe(1)
			expect(result[0]).toContain('ConnectionBuilder')
		})

		it('should handle multiple model helpers', () => {
			const helpers: ModelHelper[] = [
				{
					modelName: 'User',
					connectionBuilderName: 'UserConnectionBuilder',
					filterBuilderName: 'UserFilterBuilder',
					sortBuilderName: 'UserSortBuilder',
					fieldSelectionName: 'UserFieldSelection',
					includesConstName: 'USER_INCLUDES',
					relations: [],
				},
				{
					modelName: 'Post',
					connectionBuilderName: 'PostConnectionBuilder',
					filterBuilderName: 'PostFilterBuilder',
					sortBuilderName: 'PostSortBuilder',
					fieldSelectionName: 'PostFieldSelection',
					includesConstName: 'POST_INCLUDES',
					relations: [
						{
							modelName: 'Post',
							fieldName: 'author',
							targetModelName: 'User',
							targetFieldName: 'posts',
							isList: false,
							isRequired: true,
						},
					],
				},
			]

			const result = strategy.generateHelpers(helpers, mockContext)

			expect(result).toBeDefined()
			expect(Array.isArray(result)).toBe(true)
			expect(result.length).toBe(1)
			expect(result[0]).toContain('buildUserConnectionConfig')
			expect(result[0]).toContain('buildPostConnectionConfig')
		})

		it('should handle helpers with complex relations', () => {
			const helpers: ModelHelper[] = [
				{
					modelName: 'User',
					connectionBuilderName: 'UserConnectionBuilder',
					filterBuilderName: 'UserFilterBuilder',
					sortBuilderName: 'UserSortBuilder',
					fieldSelectionName: 'UserFieldSelection',
					includesConstName: 'USER_INCLUDES',
					relations: [
						{
							modelName: 'User',
							fieldName: 'posts',
							targetModelName: 'Post',
							targetFieldName: 'author',
							isList: true,
							isRequired: false,
						},
						{
							modelName: 'User',
							fieldName: 'comments',
							targetModelName: 'Comment',
							targetFieldName: 'author',
							isList: true,
							isRequired: false,
						},
					],
				},
			]

			const result = strategy.generateHelpers(helpers, mockContext)

			expect(result).toBeDefined()
			expect(result[0]).toContain('posts')
			expect(result[0]).toContain('comments')
		})

		it('should handle helpers with no relations', () => {
			const helpers: ModelHelper[] = [
				{
					modelName: 'User',
					connectionBuilderName: 'UserConnectionBuilder',
					filterBuilderName: 'UserFilterBuilder',
					sortBuilderName: 'UserSortBuilder',
					fieldSelectionName: 'UserFieldSelection',
					includesConstName: 'USER_INCLUDES',
					relations: [],
				},
			]

			const result = strategy.generateHelpers(helpers, mockContext)

			expect(result).toBeDefined()
			expect(result.length).toBe(1)
			expect(result[0]).toContain('USER_INCLUDES')
		})
	})

	describe('Edge Cases', () => {
		it('should handle empty helpers array', () => {
			const helpers: ModelHelper[] = []

			const result = strategy.generateHelpers(helpers, mockContext)

			expect(result).toBeDefined()
			expect(Array.isArray(result)).toBe(true)
			expect(result.length).toBe(1)
		})

		it('should handle context with no models', () => {
			const helpers: ModelHelper[] = [
				{
					modelName: 'User',
					connectionBuilderName: 'UserConnectionBuilder',
					filterBuilderName: 'UserFilterBuilder',
					sortBuilderName: 'UserSortBuilder',
					fieldSelectionName: 'UserFieldSelection',
					includesConstName: 'USER_INCLUDES',
					relations: [],
				},
			]

			const emptyContext: HelperGenerationContext = {
				...mockContext,
				models: [],
			}

			const result = strategy.generateHelpers(helpers, emptyContext)

			expect(result).toBeDefined()
			expect(result.length).toBe(1)
		})

		it('should handle malformed helper objects', () => {
			const malformedHelpers: ModelHelper[] = [
				{
					modelName: '',
					connectionBuilderName: '',
					filterBuilderName: '',
					sortBuilderName: '',
					fieldSelectionName: '',
					includesConstName: '',
					relations: [],
				},
			]

			expect(() => {
				strategy.generateHelpers(malformedHelpers, mockContext)
			}).not.toThrow()
		})

		it('should handle null relations in helpers', () => {
			const helpersWithNullRelations: ModelHelper[] = [
				{
					modelName: 'User',
					connectionBuilderName: 'UserConnectionBuilder',
					filterBuilderName: 'UserFilterBuilder',
					sortBuilderName: 'UserSortBuilder',
					fieldSelectionName: 'UserFieldSelection',
					includesConstName: 'USER_INCLUDES',
					relations: null as any,
				},
			]

			expect(() => {
				strategy.generateHelpers(helpersWithNullRelations, mockContext)
			}).toThrow()
		})

		it('should handle context with different output formats', () => {
			const helpers: ModelHelper[] = [
				{
					modelName: 'User',
					connectionBuilderName: 'UserConnectionBuilder',
					filterBuilderName: 'UserFilterBuilder',
					sortBuilderName: 'UserSortBuilder',
					fieldSelectionName: 'UserFieldSelection',
					includesConstName: 'USER_INCLUDES',
					relations: [],
				},
			]

			const typeGraphQLContext: HelperGenerationContext = {
				...mockContext,
				outputFormat: OutputFormat.TYPE_GRAPHQL,
			}

			const result1 = strategy.generateHelpers(helpers, typeGraphQLContext)
			expect(result1).toBeDefined()

			const graphqlContext: HelperGenerationContext = {
				...mockContext,
				outputFormat: OutputFormat.GRAPHQL,
			}

			const result2 = strategy.generateHelpers(helpers, graphqlContext)
			expect(result2).toBeDefined()

			expect(result1).toEqual(result2)
		})
	})

	describe('Parent Class Integration', () => {
		it('should call parent generateHelpers method', () => {
			const helpers: ModelHelper[] = [
				{
					modelName: 'User',
					connectionBuilderName: 'UserConnectionBuilder',
					filterBuilderName: 'UserFilterBuilder',
					sortBuilderName: 'UserSortBuilder',
					fieldSelectionName: 'UserFieldSelection',
					includesConstName: 'USER_INCLUDES',
					relations: [],
				},
			]

			const originalGenerateHelpers = TypeScriptHelperStrategy.prototype.generateHelpers
			let parentCalled = false

			TypeScriptHelperStrategy.prototype.generateHelpers = function (helpersArg, contextArg) {
				parentCalled = true
				expect(helpersArg).toEqual(helpers)
				expect(contextArg).toEqual(mockContext)
				return originalGenerateHelpers.call(this, helpersArg, contextArg)
			}

			const result = strategy.generateHelpers(helpers, mockContext)

			expect(parentCalled).toBe(true)
			expect(result).toBeDefined()

			TypeScriptHelperStrategy.prototype.generateHelpers = originalGenerateHelpers
		})

		it('should have access to all parent class methods', () => {

			expect(typeof strategy.generateHelpers).toBe('function')

			expect(strategy instanceof TypeScriptHelperStrategy).toBe(true)
		})

		it('should override generateHelpers correctly', () => {
			const helpers: ModelHelper[] = [
				{
					modelName: 'User',
					connectionBuilderName: 'UserConnectionBuilder',
					filterBuilderName: 'UserFilterBuilder',
					sortBuilderName: 'UserSortBuilder',
					fieldSelectionName: 'UserFieldSelection',
					includesConstName: 'USER_INCLUDES',
					relations: [],
				},
			]

			const result = strategy.generateHelpers(helpers, mockContext)

			expect(result).toBeDefined()
			expect(Array.isArray(result)).toBe(true)
			expect(result.length).toBe(1)
			expect(typeof result[0]).toBe('string')
		})
	})

	describe('Performance and Stress Tests', () => {
		it('should handle large number of helpers efficiently', () => {
			const largeHelpersArray: ModelHelper[] = []

			for (let i = 0; i < 50; i++) {
				largeHelpersArray.push({
					modelName: `Model${i}`,
					connectionBuilderName: `Model${i}ConnectionBuilder`,
					filterBuilderName: `Model${i}FilterBuilder`,
					sortBuilderName: `Model${i}SortBuilder`,
					fieldSelectionName: `Model${i}FieldSelection`,
					includesConstName: `MODEL${i}_INCLUDES`,
					relations: [],
				})
			}

			const startTime = Date.now()
			const result = strategy.generateHelpers(largeHelpersArray, mockContext)
			const endTime = Date.now()

			expect(result).toBeDefined()
			expect(endTime - startTime).toBeLessThan(1000) // Should complete within 1 second
		})

		it('should handle helpers with many relations efficiently', () => {
			const relations = []
			for (let i = 0; i < 20; i++) {
				relations.push({
					modelName: 'User',
					fieldName: `relation${i}`,
					targetModelName: `Target${i}`,
					targetFieldName: 'backRef',
					isList: i % 2 === 0,
					isRequired: i % 3 === 0,
				})
			}

			const helpers: ModelHelper[] = [
				{
					modelName: 'User',
					connectionBuilderName: 'UserConnectionBuilder',
					filterBuilderName: 'UserFilterBuilder',
					sortBuilderName: 'UserSortBuilder',
					fieldSelectionName: 'UserFieldSelection',
					includesConstName: 'USER_INCLUDES',
					relations,
				},
			]

			const result = strategy.generateHelpers(helpers, mockContext)

			expect(result).toBeDefined()
			expect(result.length).toBe(1)
		})
	})
})
