import { describe, test, expect, beforeEach } from 'bun:test'
import { SchemaComposer } from 'graphql-compose'
import { GraphQLOutputStrategy } from '@generators/strategies/graphql-output-strategy'
import { GraphQLRegistry } from '@utils/registry'
import { GraphQLTypeFactories } from '@utils/schema/graphql-type-factories'
import { OutputFormat } from '@utils/constants'
import { TestUtils, MockFactory } from '../../helpers'
import { CommonTypeDefinition, SortFieldDefinition, FilterFieldDefinition } from '@generators/strategies/output-strategy'
import { ErrorCategory, PluginError } from '@utils/error'
import { TypeKind } from '@utils/registry'

describe('GraphQL Output Strategy', () => {
	let strategy: GraphQLOutputStrategy
	let registry: GraphQLRegistry
	let schemaComposer: SchemaComposer<unknown>
	let typeFactories: GraphQLTypeFactories
	let options: any

	beforeEach(() => {
		const baseContext = TestUtils.createMockContext({
			outputFormat: OutputFormat.GRAPHQL,
			scalarTypes: { DateTime: 'DateTime' },
		})

		const mockContext = MockFactory.createMockGraphQLContext(baseContext)

		registry = mockContext.registry
		schemaComposer = mockContext.schemaComposer
		typeFactories = mockContext.typeFactories
		options = mockContext.options

		strategy = new GraphQLOutputStrategy(registry, schemaComposer, typeFactories, options)
	})

	describe('Initialization', () => {
		test('should initialize with required dependencies', () => {
			expect(strategy).toBeDefined()
			expect(strategy).toBeInstanceOf(GraphQLOutputStrategy)
		})

		test('should handle empty options', () => {
			const minimalOptions = {}
			const minimalStrategy = new GraphQLOutputStrategy(registry, schemaComposer, typeFactories, minimalOptions)
			expect(minimalStrategy).toBeDefined()
		})
	})

	describe('Common Types Creation', () => {
		test('should create enum types from common type definitions', () => {
			const enumDefinition: CommonTypeDefinition = {
				name: 'UserRole',
				type: 'enum',
				definition: {
					name: 'UserRole',
					values: { ADMIN: { value: 'ADMIN' }, USER: { value: 'USER' } },
				},
			}

			strategy.createCommonTypes([enumDefinition])

			expect(strategy.hasType('UserRole')).toBe(true)
		})

		test('should create input types from common type definitions', () => {
			const inputDefinition: CommonTypeDefinition = {
				name: 'UserInput',
				type: 'input',
				definition: {
					name: 'UserInput',
					fields: { name: { type: 'String' } },
				},
			}

			strategy.createCommonTypes([inputDefinition])

			expect(strategy.hasType('UserInput')).toBe(true)
		})

		test('should skip already existing types', () => {
			const enumDefinition: CommonTypeDefinition = {
				name: 'UserRole',
				type: 'enum',
				definition: {
					name: 'UserRole',
					values: { ADMIN: { value: 'ADMIN' } },
				},
			}

			strategy.createCommonTypes([enumDefinition])
			const typesCountAfterFirst = registry.getTypeNamesByKind(TypeKind.ENUM).length

			strategy.createCommonTypes([enumDefinition])
			const typesCountAfterSecond = registry.getTypeNamesByKind(TypeKind.ENUM).length

			expect(typesCountAfterFirst).toBe(typesCountAfterSecond)
		})

		test('should handle multiple type definitions', () => {
			const definitions: CommonTypeDefinition[] = [
				{
					name: 'UserRole',
					type: 'enum',
					definition: {
						name: 'UserRole',
						values: { ADMIN: { value: 'ADMIN' } },
					},
				},
				{
					name: 'UserInput',
					type: 'input',
					definition: {
						name: 'UserInput',
						fields: { name: { type: 'String' } },
					},
				},
			]

			strategy.createCommonTypes(definitions)

			expect(strategy.hasType('UserRole')).toBe(true)
			expect(strategy.hasType('UserInput')).toBe(true)
		})
	})

	describe('Enum Type Creation', () => {
		test('should create enum type with fields', () => {
			const enumMock = TestUtils.createMockEnum('Priority', ['HIGH', 'MEDIUM', 'LOW'])

			const result = strategy.createEnumType(enumMock)

			expect(result).toBe('Priority')
			expect(strategy.hasType('Priority')).toBe(true)
		})

		test('should return existing enum name if already created', () => {
			const enumMock = TestUtils.createMockEnum('Priority', ['HIGH', 'MEDIUM', 'LOW'])

			const firstResult = strategy.createEnumType(enumMock)
			const secondResult = strategy.createEnumType(enumMock)

			expect(firstResult).toBe(secondResult)
			expect(firstResult).toBe('Priority')
		})

		test('should handle enum with no fields', () => {
			const enumMock = TestUtils.createMockEnum('EmptyEnum', [])

			const result = strategy.createEnumType(enumMock)

			expect(result).toBe('EmptyEnum')
			expect(strategy.hasType('EmptyEnum')).toBe(true)
		})

		test('should handle enum with comments', () => {
			const enumMock = {
				...TestUtils.createMockEnum('Priority', ['HIGH', 'LOW']),
				comments: ['Priority level for tasks'],
			}

			const result = strategy.createEnumType(enumMock)

			expect(result).toBe('Priority')
			expect(strategy.hasType('Priority')).toBe(true)
		})
	})

	describe('Sort Input Type Creation', () => {
		test('should create sort input type with fields', () => {
			const fields: SortFieldDefinition[] = [
				{ name: 'name', description: 'Sort by name' },
				{ name: 'createdAt', description: 'Sort by creation date' },
			]

			const result = strategy.createSortInputType('User', fields)

			expect(result).toBe('UserSortInput')
			expect(registry.hasType('UserSortInput')).toBe(true)
		})

		test('should return existing sort input if already created', () => {
			const fields: SortFieldDefinition[] = [{ name: 'name' }]

			const firstResult = strategy.createSortInputType('User', fields)
			const secondResult = strategy.createSortInputType('User', fields)

			expect(firstResult).toBe(secondResult)
			expect(firstResult).toBe('UserSortInput')
		})

		test('should handle empty fields array', () => {
			const result = strategy.createSortInputType('User', [])

			expect(result).toBe('UserSortInput')
			expect(registry.hasType('UserSortInput')).toBe(true)
		})

		test('should handle fields without description', () => {
			const fields: SortFieldDefinition[] = [{ name: 'name' }, { name: 'email' }]

			const result = strategy.createSortInputType('User', fields)

			expect(result).toBe('UserSortInput')
			expect(registry.hasType('UserSortInput')).toBe(true)
		})

		test('should throw PluginError on type factory failure', () => {
			const mockTypeFactories = {
				createSortInputType: () => {
					throw new Error('Type factory error')
				},
			} as any

			const failingStrategy = new GraphQLOutputStrategy(registry, schemaComposer, mockTypeFactories, options)

			expect(() => {
				failingStrategy.createSortInputType('User', [{ name: 'test' }])
			}).toThrow(PluginError)
		})
	})

	describe('Filter Input Type Creation', () => {
		test('should create filter input type with fields', () => {
			const fields: FilterFieldDefinition[] = [
				{ name: 'name', type: 'String', description: 'Filter by name' },
				{ name: 'age', type: 'Int', nullable: false },
			]

			const result = strategy.createFilterInputType('User', fields)

			expect(result).toBe('UserFilterInput')
			expect(schemaComposer.has('UserFilterInput')).toBe(true)
		})

		test('should add AND/OR logical operators', () => {
			const fields: FilterFieldDefinition[] = [{ name: 'name', type: 'String' }]

			strategy.createFilterInputType('User', fields)

			expect(schemaComposer.has('UserFilterInput')).toBe(true)
			const filterInput = schemaComposer.getITC('UserFilterInput')
			expect(filterInput.hasField('AND')).toBe(true)
			expect(filterInput.hasField('OR')).toBe(true)
		})

		test('should return existing filter input if already created', () => {
			const fields: FilterFieldDefinition[] = [{ name: 'name', type: 'String' }]

			const firstResult = strategy.createFilterInputType('User', fields)
			const secondResult = strategy.createFilterInputType('User', fields)

			expect(firstResult).toBe(secondResult)
			expect(firstResult).toBe('UserFilterInput')
		})

		test('should handle empty fields array', () => {
			const result = strategy.createFilterInputType('User', [])

			expect(result).toBe('UserFilterInput')
			expect(schemaComposer.has('UserFilterInput')).toBe(false)
		})

		test('should handle fields without description', () => {
			const fields: FilterFieldDefinition[] = [
				{ name: 'name', type: 'String' },
				{ name: 'active', type: 'Boolean' },
			]

			const result = strategy.createFilterInputType('User', fields)

			expect(result).toBe('UserFilterInput')
			expect(schemaComposer.has('UserFilterInput')).toBe(true)
		})
	})

	describe('Connection Type Creation', () => {
		test('should create connection type', () => {
			const result = strategy.createConnectionType('User')

			expect(result).toBe('UserConnection')
			expect(registry.hasType('UserConnection')).toBe(true)
		})

		test('should return existing connection if already created', () => {
			const firstResult = strategy.createConnectionType('User')
			const secondResult = strategy.createConnectionType('User')

			expect(firstResult).toBe(secondResult)
			expect(firstResult).toBe('UserConnection')
		})

		test('should throw PluginError on type factory failure', () => {
			const mockTypeFactories = {
				createConnectionType: () => {
					throw new Error('Connection error')
				},
			} as any

			const failingStrategy = new GraphQLOutputStrategy(registry, schemaComposer, mockTypeFactories, options)

			expect(() => {
				failingStrategy.createConnectionType('User')
			}).toThrow(PluginError)
		})
	})

	describe('Object Type Creation', () => {
		test('should create object type with fields', () => {
			const fields = {
				name: { type: 'String' },
				age: { type: 'Int' },
			}

			const result = strategy.createObjectType('User', fields, 'User object type')

			expect(result).toBe('User')
			expect(schemaComposer.has('User')).toBe(true)
		})

		test('should return existing object type if already created', () => {
			const fields = { name: { type: 'String' } }

			const firstResult = strategy.createObjectType('User', fields)
			const secondResult = strategy.createObjectType('User', fields)

			expect(firstResult).toBe(secondResult)
			expect(firstResult).toBe('User')
		})

		test('should handle creation without description', () => {
			const fields = { id: { type: 'ID' } }

			const result = strategy.createObjectType('User', fields)

			expect(result).toBe('User')
			expect(schemaComposer.has('User')).toBe(true)
		})

		test('should throw PluginError on object creation failure', () => {
			const mockComposer = {
				has: () => false,
				createObjectTC: () => {
					throw new Error('Object creation error')
				},
			} as any

			const failingStrategy = new GraphQLOutputStrategy(registry, mockComposer, typeFactories, options)

			expect(() => {
				failingStrategy.createObjectType('User', { name: { type: 'String' } })
			}).toThrow(PluginError)
		})
	})

	describe('Pagination Types Creation', () => {
		test('should create PageInfo type when not exists', () => {
			strategy.createPaginationTypes()

			expect(registry.hasType('PageInfo')).toBe(true)
		})

		test('should not recreate PageInfo if already exists', () => {
			strategy.createPaginationTypes()
			const typesCountAfterFirst = registry.getTypeNamesByKind(TypeKind.OBJECT).length

			strategy.createPaginationTypes()
			const typesCountAfterSecond = registry.getTypeNamesByKind(TypeKind.OBJECT).length

			expect(typesCountAfterFirst).toBe(typesCountAfterSecond)
		})

		test('should create pagination input types', () => {
			strategy.createPaginationTypes()

			const inputTypes = registry.getTypeNamesByKind(TypeKind.INPUT)
			expect(inputTypes.length).toBeGreaterThan(0)
		})
	})

	describe('Common Filter Types Creation', () => {
		test('should create common filter types', () => {
			strategy.createCommonFilterTypes()

			const filterTypes = registry.getTypeNamesByKind(TypeKind.INPUT)
			expect(filterTypes.length).toBeGreaterThan(0)
		})

		test('should handle DateTime scalar type configuration', () => {
			const customOptions = {
				...options,
				scalarTypes: { DateTime: 'CustomDateTime' },
			}
			const customStrategy = new GraphQLOutputStrategy(registry, schemaComposer, typeFactories, customOptions)

			customStrategy.createCommonFilterTypes()

			expect(schemaComposer.has('CustomDateTimeFilterInput')).toBe(true)
		})

		test('should not recreate existing filter types', () => {
			strategy.createCommonFilterTypes()
			const typesCountAfterFirst = registry.getTypeNamesByKind(TypeKind.INPUT).length

			strategy.createCommonFilterTypes()
			const typesCountAfterSecond = registry.getTypeNamesByKind(TypeKind.INPUT).length

			expect(typesCountAfterFirst).toBe(typesCountAfterSecond)
		})
	})

	describe('Sort Direction Enum Creation', () => {
		test('should create SortDirection enum', () => {
			strategy.createSortDirectionEnum()

			expect(registry.isTypeOfKind('SortDirection', TypeKind.ENUM)).toBe(true)
		})

		test('should not recreate if already exists', () => {
			strategy.createSortDirectionEnum()
			strategy.createSortDirectionEnum()

			expect(registry.isTypeOfKind('SortDirection', TypeKind.ENUM)).toBe(true)
		})

		test('should throw PluginError on creation failure', () => {
			const freshSchemaComposer = new SchemaComposer()
			const freshRegistry = new GraphQLRegistry(freshSchemaComposer)

			const mockTypeFactories = {
				createSortDirectionEnum: () => {
					throw new Error('Sort direction error')
				},
			} as any

			const failingStrategy = new GraphQLOutputStrategy(freshRegistry, freshSchemaComposer, mockTypeFactories, options)

			expect(() => {
				failingStrategy.createSortDirectionEnum()
			}).toThrow(PluginError)
		})
	})

	describe('Query Args Input Type Creation', () => {
		test('should create query args input type', () => {
			const model = TestUtils.createMockDataModel('User')

			const result = strategy.createQueryArgsInputType('UserQueryArgs', model)

			expect(result).toBe('UserQueryArgs')
			expect(schemaComposer.has('UserQueryArgs')).toBe(true)
		})

		test('should include pagination fields', () => {
			const model = TestUtils.createMockDataModel('User')

			strategy.createQueryArgsInputType('UserQueryArgs', model)

			const queryArgs = schemaComposer.getITC('UserQueryArgs')
			expect(queryArgs.hasField('first')).toBe(true)
			expect(queryArgs.hasField('after')).toBe(true)
			expect(queryArgs.hasField('last')).toBe(true)
			expect(queryArgs.hasField('before')).toBe(true)
			expect(queryArgs.hasField('connection')).toBe(true)
		})

		test('should include filter and sort if they exist', () => {
			const model = TestUtils.createMockDataModel('User')

			strategy.createFilterInputType('User', [{ name: 'name', type: 'String' }])
			strategy.createSortInputType('User', [{ name: 'name' }])

			strategy.createQueryArgsInputType('UserQueryArgs', model)

			const queryArgs = schemaComposer.getITC('UserQueryArgs')
			expect(queryArgs.hasField('filter')).toBe(true)
			expect(queryArgs.hasField('sort')).toBe(true)
		})

		test('should return existing if already created', () => {
			const model = TestUtils.createMockDataModel('User')

			const firstResult = strategy.createQueryArgsInputType('UserQueryArgs', model)
			const secondResult = strategy.createQueryArgsInputType('UserQueryArgs', model)

			expect(firstResult).toBe(secondResult)
			expect(firstResult).toBe('UserQueryArgs')
		})

		test('should throw PluginError on creation failure', () => {
			const model = TestUtils.createMockDataModel('User')
			const mockComposer = {
				has: () => false,
				createInputTC: () => {
					throw new Error('Query args creation error')
				},
			} as any

			const failingStrategy = new GraphQLOutputStrategy(registry, mockComposer, typeFactories, options)

			expect(() => {
				failingStrategy.createQueryArgsInputType('UserQueryArgs', model)
			}).toThrow(PluginError)
		})
	})

	describe('Type Checking', () => {
		test('should check if type exists in composer or registry', () => {
			schemaComposer.createObjectTC({ name: 'ExistingType', fields: {} })

			expect(strategy.hasType('ExistingType')).toBe(true)
			expect(strategy.hasType('NonExistentType')).toBe(false)
		})

		test('should check type existence in registry', () => {
			registry.registerType('RegistryType', TypeKind.OBJECT, schemaComposer.createObjectTC({ name: 'RegistryType', fields: {} }), true)

			expect(strategy.hasType('RegistryType')).toBe(true)
		})
	})

	describe('Relation Processing', () => {
		test('should check processed relations', () => {
			const relationKey = 'User_posts_Post'

			expect(strategy.hasProcessedRelation(relationKey)).toBe(false)

			registry.addProcessedRelation(relationKey)

			expect(strategy.hasProcessedRelation(relationKey)).toBe(true)
		})

		test('should get processed relations', () => {
			const relationKey = 'User_posts_Post'
			registry.addProcessedRelation(relationKey)

			const processedRelations = strategy.getProcessedRelations()

			expect(processedRelations).toContain(relationKey)
		})
	})

	describe('Generated Type Names', () => {
		test('should get all generated input type names', () => {
			strategy.createFilterInputType('User', [{ name: 'name', type: 'String' }])
			strategy.createSortInputType('User', [{ name: 'name' }])

			const typeNames = strategy.getGeneratedTypeNames()

			expect(typeNames).toContain('UserFilterInput')
			expect(typeNames).toContain('UserSortInput')
		})

		test('should filter generated type names', () => {
			strategy.createFilterInputType('User', [{ name: 'name', type: 'String' }])
			strategy.createSortInputType('Post', [{ name: 'title' }])

			const filteredTypes = strategy.getGeneratedTypeNames((name) => name.includes('User'))

			expect(filteredTypes).toContain('UserFilterInput')
			expect(filteredTypes).not.toContain('PostSortInput')
		})
	})

	describe('Error Handling', () => {
		test('should handle errors gracefully in enum creation', () => {
			const invalidEnum = { name: '', fields: [] }

			expect(() => {
				strategy.createEnumType(invalidEnum)
			}).toThrow()
		})

		test('should validate PluginError instances', () => {
			const mockTypeFactories = {
				createConnectionType: () => {
					throw new Error('Test error')
				},
			} as any

			const failingStrategy = new GraphQLOutputStrategy(registry, schemaComposer, mockTypeFactories, options)

			try {
				failingStrategy.createConnectionType('User')
			} catch (error) {
				expect(error).toBeInstanceOf(PluginError)
				expect((error as PluginError).category).toBe(ErrorCategory.GENERATION)
			}
		})
	})

	describe('Input Type Creation', () => {
		test('should return empty string for createInputType', () => {
			const model = TestUtils.createMockDataModel('User')

			const result = strategy.createInputType('UserInput', model, 'create')

			expect(result).toBe('')
		})
	})
})
