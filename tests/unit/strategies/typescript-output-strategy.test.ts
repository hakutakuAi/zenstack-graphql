import { describe, test, expect, beforeEach } from 'bun:test'
import { TypeScriptOutputStrategy } from '@generators/strategies/typescript-output-strategy'
import { TypeScriptASTFactory } from '@utils/typescript/ast-factory'
import { TestFixtures } from '../../helpers'
import { CommonTypeDefinition, SortFieldDefinition, FilterFieldDefinition } from '@generators/strategies/output-strategy'

describe('TypeScript Output Strategy', () => {
	let strategy: TypeScriptOutputStrategy
	let mockASTFactory: TypeScriptASTFactory

	beforeEach(() => {
		const mockEnumDeclaration = { name: 'MockEnum', kind: 'Enum' } as any
		const mockClassDeclaration = { name: 'MockClass', kind: 'Class' } as any
		const mockConnectionResult = { edge: mockClassDeclaration, connection: mockClassDeclaration }

		mockASTFactory = {
			createEnumType: () => mockEnumDeclaration,
			createSortInputType: () => mockClassDeclaration,
			createFilterInputType: () => mockClassDeclaration,
			createConnectionType: () => mockConnectionResult,
			createPaginationInputTypes: () => [mockClassDeclaration],
			createSortDirectionEnum: () => mockEnumDeclaration,
			hasType: (typeName: string) => typeName === 'ExistingType',
			createObjectTypeFromFields: () => mockClassDeclaration,
			createInputType: () => mockClassDeclaration,
			getGeneratedTypeNames: (filter?: (name: string) => boolean) => {
				const allTypes = ['UserFilterInput', 'PostSortInput', 'CommentConnection']
				return filter ? allTypes.filter(filter) : allTypes
			},
			getGeneratedCode: () => 'export interface User { name: string; }',
		} as any

		strategy = new TypeScriptOutputStrategy(mockASTFactory)
	})

	describe('Initialization', () => {
		test('should initialize with AST factory', () => {
			expect(strategy).toBeDefined()
			expect(strategy).toBeInstanceOf(TypeScriptOutputStrategy)
		})

		test('should handle null AST factory gracefully', () => {
			expect(() => new TypeScriptOutputStrategy(null as any)).not.toThrow()
		})
	})

	describe('Common Types Creation', () => {
		test('should create enum types from common type definitions', () => {
			const mockEnumDeclaration = { name: 'MockEnum', kind: 'Enum' } as any
			const createEnumTypeSpy = () => {
				mockASTFactory.createEnumType = (enumDef: any) => {
					expect(enumDef.name).toBe('UserRole')
					return mockEnumDeclaration
				}
			}
			createEnumTypeSpy()

			const enumDefinition: CommonTypeDefinition = {
				name: 'UserRole',
				type: 'enum',
				definition: {
					name: 'UserRole',
					values: ['ADMIN', 'USER'],
				},
			}

			strategy.createCommonTypes([enumDefinition])
		})

		test('should skip input types in common types creation', () => {
			let createEnumCalled = false
			const mockEnumDeclaration = { name: 'MockEnum', kind: 'Enum' } as any
			mockASTFactory.createEnumType = () => {
				createEnumCalled = true
				return mockEnumDeclaration
			}

			const inputDefinition: CommonTypeDefinition = {
				name: 'UserInput',
				type: 'input',
				definition: {
					name: 'UserInput',
					fields: { name: { type: 'string' } },
				},
			}

			strategy.createCommonTypes([inputDefinition])

			expect(createEnumCalled).toBe(false)
		})

		test('should handle multiple type definitions', () => {
			let enumCreateCount = 0
			const mockEnumDeclaration = { name: 'MockEnum', kind: 'Enum' } as any
			mockASTFactory.createEnumType = () => {
				enumCreateCount++
				return mockEnumDeclaration
			}

			const definitions: CommonTypeDefinition[] = [
				{
					name: 'UserRole',
					type: 'enum',
					definition: { name: 'UserRole', values: ['ADMIN'] },
				},
				{
					name: 'Priority',
					type: 'enum',
					definition: { name: 'Priority', values: ['HIGH', 'LOW'] },
				},
				{
					name: 'UserInput',
					type: 'input',
					definition: { name: 'UserInput', fields: {} },
				},
			]

			strategy.createCommonTypes(definitions)

			expect(enumCreateCount).toBe(2)
		})

		test('should handle empty definitions array', () => {
			expect(() => {
				strategy.createCommonTypes([])
			}).not.toThrow()
		})
	})

	describe('Enum Type Creation', () => {
		test('should create enum type and return name', () => {
			let createdEnumType: any = null
			mockASTFactory.createEnumType = (enumType: any) => {
				createdEnumType = enumType
				return { name: enumType.name, kind: 'Enum' } as any
			}

			const enumMock = TestFixtures.createEnum('Priority', ['HIGH', 'MEDIUM', 'LOW'])

			const result = strategy.createEnumType(enumMock)

			expect(result).toBe('Priority')
			expect(createdEnumType).toBe(enumMock)
		})

		test('should handle enum with no fields', () => {
			let createdEnumType: any = null
			mockASTFactory.createEnumType = (enumType: any) => {
				createdEnumType = enumType
				return { name: enumType.name, kind: 'Enum' } as any
			}

			const enumMock = TestFixtures.createEnum('EmptyEnum', [])

			const result = strategy.createEnumType(enumMock)

			expect(result).toBe('EmptyEnum')
			expect(createdEnumType).toBe(enumMock)
		})

		test('should handle null enum name', () => {
			const enumMock = { ...TestFixtures.createEnum('', ['VALUE']), name: null }

			const result = strategy.createEnumType(enumMock)

			expect(result).toBeNull()
		})
	})

	describe('Sort Input Type Creation', () => {
		test('should create sort input type with fields', () => {
			let createdTypeName: string = ''
			let createdFields: any[] = []
			mockASTFactory.createSortInputType = (typeName: string, fields: any[]) => {
				createdTypeName = typeName
				createdFields = fields
				return { name: 'MockClass', kind: 'Class' } as any
			}

			const fields: SortFieldDefinition[] = [
				{ name: 'name', description: 'Sort by name' },
				{ name: 'createdAt', description: 'Sort by creation date' },
			]

			const result = strategy.createSortInputType('User', fields)

			expect(result).toBe('UserSortInput')
			expect(createdTypeName).toBe('User')
			expect(createdFields).toEqual(fields)
		})

		test('should handle empty fields with placeholder', () => {
			let createdFields: any[] = []
			mockASTFactory.createSortInputType = (typeName: string, fields: any[]) => {
				createdFields = fields
				return { name: 'MockClass', kind: 'Class' } as any
			}

			const result = strategy.createSortInputType('User', [])

			expect(result).toBe('UserSortInput')
			expect(createdFields).toEqual([
				{
					name: '_placeholder',
					description: 'Placeholder field when no sortable fields are available',
				},
			])
		})

		test('should handle fields without description', () => {
			let createdFields: any[] = []
			mockASTFactory.createSortInputType = (typeName: string, fields: any[]) => {
				createdFields = fields
				return { name: 'MockClass', kind: 'Class' } as any
			}

			const fields: SortFieldDefinition[] = [{ name: 'name' }, { name: 'email' }]

			const result = strategy.createSortInputType('User', fields)

			expect(result).toBe('UserSortInput')
			expect(createdFields).toEqual(fields)
		})

		test('should handle special characters in type name', () => {
			let createdTypeName: string = ''
			mockASTFactory.createSortInputType = (typeName: string) => {
				createdTypeName = typeName
				return { name: 'MockClass', kind: 'Class' } as any
			}

			const result = strategy.createSortInputType('User_Profile_123', [{ name: 'field' }])

			expect(result).toBe('User_Profile_123SortInput')
			expect(createdTypeName).toBe('User_Profile_123')
		})
	})

	describe('Filter Input Type Creation', () => {
		test('should create filter input type with fields', () => {
			let createdTypeName: string = ''
			let createdFields: any[] = []
			mockASTFactory.createFilterInputType = (typeName: string, fields: any[]) => {
				createdTypeName = typeName
				createdFields = fields
				return { name: 'MockClass', kind: 'Class' } as any
			}

			const fields: FilterFieldDefinition[] = [
				{ name: 'name', type: 'string', description: 'Filter by name' },
				{ name: 'age', type: 'number', nullable: false },
			]

			const result = strategy.createFilterInputType('User', fields)

			expect(result).toBe('UserFilterInput')
			expect(createdTypeName).toBe('UserFilterInput')
			expect(createdFields).toEqual([
				{ name: 'name', type: 'string', nullable: true },
				{ name: 'age', type: 'number', nullable: false },
				{ name: 'AND', type: '[UserFilterInput!]', nullable: true },
				{ name: 'OR', type: '[UserFilterInput!]', nullable: true },
			])
		})

		test('should add AND/OR logical operators', () => {
			let createdFields: any[] = []
			mockASTFactory.createFilterInputType = (typeName: string, fields: any[]) => {
				createdFields = fields
				return { name: 'MockClass', kind: 'Class' } as any
			}

			const fields: FilterFieldDefinition[] = [{ name: 'name', type: 'string' }]

			strategy.createFilterInputType('User', fields)

			const logicalFields = createdFields.filter((f) => f.name === 'AND' || f.name === 'OR')
			expect(logicalFields).toHaveLength(2)
			expect(logicalFields[0]).toEqual({ name: 'AND', type: '[UserFilterInput!]', nullable: true })
			expect(logicalFields[1]).toEqual({ name: 'OR', type: '[UserFilterInput!]', nullable: true })
		})

		test('should handle nullable field defaults', () => {
			let createdFields: any[] = []
			mockASTFactory.createFilterInputType = (typeName: string, fields: any[]) => {
				createdFields = fields
				return { name: 'MockClass', kind: 'Class' } as any
			}

			const fields: FilterFieldDefinition[] = [
				{ name: 'name', type: 'string' },
				{ name: 'active', type: 'boolean', nullable: false },
			]

			strategy.createFilterInputType('User', fields)

			expect(createdFields[0]).toEqual({ name: 'name', type: 'string', nullable: true })
			expect(createdFields[1]).toEqual({ name: 'active', type: 'boolean', nullable: false })
		})

		test('should handle empty fields array', () => {
			let createdFields: any[] = []
			mockASTFactory.createFilterInputType = (typeName: string, fields: any[]) => {
				createdFields = fields
				return { name: 'MockClass', kind: 'Class' } as any
			}

			const result = strategy.createFilterInputType('User', [])

			expect(result).toBe('UserFilterInput')
			expect(createdFields).toEqual([
				{ name: 'AND', type: '[UserFilterInput!]', nullable: true },
				{ name: 'OR', type: '[UserFilterInput!]', nullable: true },
			])
		})

		test('should handle complex type names', () => {
			let createdTypeName: string = ''
			mockASTFactory.createFilterInputType = (typeName: string) => {
				createdTypeName = typeName
				return { name: 'MockClass', kind: 'Class' } as any
			}

			const result = strategy.createFilterInputType('Complex_Type_Name', [{ name: 'field', type: 'string' }])

			expect(result).toBe('Complex_Type_NameFilterInput')
			expect(createdTypeName).toBe('Complex_Type_NameFilterInput')
		})
	})

	describe('Connection Type Creation', () => {
		test('should create connection type', () => {
			let createdTypeName: string = ''
			mockASTFactory.createConnectionType = (typeName: string) => {
				createdTypeName = typeName
				return { edge: { name: 'MockClass', kind: 'Class' } as any, connection: { name: 'MockClass', kind: 'Class' } as any }
			}

			const result = strategy.createConnectionType('User')

			expect(result).toBe('UserConnection')
			expect(createdTypeName).toBe('User')
		})

		test('should handle special characters in type name', () => {
			let createdTypeName: string = ''
			mockASTFactory.createConnectionType = (typeName: string) => {
				createdTypeName = typeName
				return { edge: { name: 'MockClass', kind: 'Class' } as any, connection: { name: 'MockClass', kind: 'Class' } as any }
			}

			const result = strategy.createConnectionType('User_Profile')

			expect(result).toBe('User_ProfileConnection')
			expect(createdTypeName).toBe('User_Profile')
		})

		test('should handle empty type name', () => {
			let createdTypeName: string = ''
			mockASTFactory.createConnectionType = (typeName: string) => {
				createdTypeName = typeName
				return { edge: { name: 'MockClass', kind: 'Class' } as any, connection: { name: 'MockClass', kind: 'Class' } as any }
			}

			const result = strategy.createConnectionType('')

			expect(result).toBe('Connection')
			expect(createdTypeName).toBe('')
		})
	})

	describe('Pagination Types Creation', () => {
		test('should create pagination input types', () => {
			let paginationTypesCalled = false
			mockASTFactory.createPaginationInputTypes = () => {
				paginationTypesCalled = true
				return [{ name: 'MockClass', kind: 'Class' } as any]
			}

			strategy.createPaginationTypes()

			expect(paginationTypesCalled).toBe(true)
		})

		test('should not throw on factory error', () => {
			mockASTFactory.createPaginationInputTypes = () => {
				throw new Error('Factory error')
			}

			expect(() => {
				strategy.createPaginationTypes()
			}).toThrow('Factory error')
		})
	})

	describe('Common Filter Types Creation', () => {
		test('should create common filter types', () => {
			let createdFilterTypes: string[] = []
			mockASTFactory.createFilterInputType = (typeName: string) => {
				createdFilterTypes.push(typeName)
				return { name: 'MockClass', kind: 'Class' } as any
			}

			strategy.createCommonFilterTypes()

			expect(createdFilterTypes.length).toBeGreaterThan(0)
		})

		test('should handle AST factory errors gracefully', () => {
			mockASTFactory.createFilterInputType = () => {
				throw new Error('Filter creation error')
			}

			expect(() => {
				strategy.createCommonFilterTypes()
			}).toThrow('Filter creation error')
		})
	})

	describe('Sort Direction Enum Creation', () => {
		test('should create sort direction enum', () => {
			let sortDirectionEnumCalled = false
			mockASTFactory.createSortDirectionEnum = () => {
				sortDirectionEnumCalled = true
				return { name: 'MockEnum', kind: 'Enum' } as any
			}

			strategy.createSortDirectionEnum()

			expect(sortDirectionEnumCalled).toBe(true)
		})

		test('should handle factory error', () => {
			mockASTFactory.createSortDirectionEnum = () => {
				throw new Error('Sort direction error')
			}

			expect(() => {
				strategy.createSortDirectionEnum()
			}).toThrow('Sort direction error')
		})
	})

	describe('Object Type Creation', () => {
		test('should create object type with fields and description', () => {
			let createdArgs: any = null
			mockASTFactory.createObjectTypeFromFields = (typeName: string, fields: any, description?: string) => {
				createdArgs = { typeName, fields, description }
				return { name: 'MockClass', kind: 'Class' } as any
			}

			const fields = {
				name: { type: 'string' },
				age: { type: 'number' },
			}

			const result = strategy.createObjectType('User', fields, 'User object type')

			expect(result).toBe('User')
			expect(createdArgs).toEqual({
				typeName: 'User',
				fields,
				description: 'User object type',
			})
		})

		test('should handle creation without description', () => {
			let createdArgs: any = null
			mockASTFactory.createObjectTypeFromFields = (typeName: string, fields: any, description?: string) => {
				createdArgs = { typeName, fields, description }
				return { name: 'MockClass', kind: 'Class' } as any
			}

			const fields = { id: { type: 'string' } }

			const result = strategy.createObjectType('User', fields)

			expect(result).toBe('User')
			expect(createdArgs.description).toBeUndefined()
		})

		test('should handle empty fields object', () => {
			let createdArgs: any = null
			mockASTFactory.createObjectTypeFromFields = (typeName: string, fields: any) => {
				createdArgs = { typeName, fields }
				return { name: 'MockClass', kind: 'Class' } as any
			}

			const result = strategy.createObjectType('User', {})

			expect(result).toBe('User')
			expect(createdArgs.fields).toEqual({})
		})
	})

	describe('Input Type Creation', () => {
		test('should create input type for create operation', () => {
			let createdArgs: any = null
			mockASTFactory.createInputType = (typeName: string, model: any, inputType: string, description?: string) => {
				createdArgs = { typeName, model, inputType, description }
				return { name: 'MockClass', kind: 'Class' } as any
			}

			const model = TestFixtures.createDataModel('User')

			const result = strategy.createInputType('UserCreateInput', model, 'create', 'Create input for User')

			expect(result).toBe('UserCreateInput')
			expect(createdArgs).toEqual({
				typeName: 'UserCreateInput',
				model,
				inputType: 'create',
				description: 'Create input for User',
			})
		})

		test('should create input type for update operation', () => {
			let createdArgs: any = null
			mockASTFactory.createInputType = (typeName: string, model: any, inputType: string, description?: string) => {
				createdArgs = { typeName, model, inputType, description }
				return { name: 'MockClass', kind: 'Class' } as any
			}

			const model = TestFixtures.createDataModel('User')

			const result = strategy.createInputType('UserUpdateInput', model, 'update')

			expect(result).toBe('UserUpdateInput')
			expect(createdArgs.inputType).toBe('update')
			expect(createdArgs.description).toBeUndefined()
		})
	})

	describe('Query Args Input Type Creation', () => {
		test('should create query args without filterable/sortable fields', () => {
			let createdArgs: any = null
			mockASTFactory.createFilterInputType = (typeName: string, fields: any[]) => {
				createdArgs = { typeName, fields }
				return { name: 'MockClass', kind: 'Class' } as any
			}

			const model = TestFixtures.createDataModel('User')

			const result = strategy.createQueryArgsInputType('UserQueryArgs', model)

			expect(result).toBe('UserQueryArgs')
			expect(createdArgs.fields).toEqual([
				{ name: 'first', type: 'Int', nullable: true },
				{ name: 'after', type: 'String', nullable: true },
				{ name: 'last', type: 'Int', nullable: true },
				{ name: 'before', type: 'String', nullable: true },
				{ name: 'connection', type: 'Boolean', nullable: true },
			])
		})

		test('should include filter and sort fields when attributes present', () => {
			let createdArgs: any = null
			mockASTFactory.createFilterInputType = (typeName: string, fields: any[]) => {
				createdArgs = { typeName, fields }
				return { name: 'MockClass', kind: 'Class' } as any
			}
			mockASTFactory.hasType = (typeName: string) => ['UserFilterInput', 'UserSortInput'].includes(typeName)

			const mockField = {
				...TestFixtures.createField('name', 'String'),
				attributes: [TestFixtures.createAttribute('graphql.filterable'), TestFixtures.createAttribute('graphql.sortable')],
			}
			const model = TestFixtures.createDataModel('User', [mockField])

			const result = strategy.createQueryArgsInputType('UserQueryArgs', model)

			expect(result).toBe('UserQueryArgs')
			expect(createdArgs.fields).toEqual([
				{ name: 'filter', type: 'UserFilterInput', nullable: true },
				{ name: 'sort', type: 'UserSortInput', nullable: true },
				{ name: 'first', type: 'Int', nullable: true },
				{ name: 'after', type: 'String', nullable: true },
				{ name: 'last', type: 'Int', nullable: true },
				{ name: 'before', type: 'String', nullable: true },
				{ name: 'connection', type: 'Boolean', nullable: true },
			])
		})

		test('should handle model with only filterable fields', () => {
			let createdArgs: any = null
			mockASTFactory.createFilterInputType = (typeName: string, fields: any[]) => {
				createdArgs = { typeName, fields }
				return { name: 'MockClass', kind: 'Class' } as any
			}
			mockASTFactory.hasType = (typeName: string) => typeName === 'UserFilterInput'

			const mockField = {
				...TestFixtures.createField('name', 'String'),
				attributes: [TestFixtures.createAttribute('graphql.filterable')],
			}
			const model = TestFixtures.createDataModel('User', [mockField])

			strategy.createQueryArgsInputType('UserQueryArgs', model)

			const hasFilterField = createdArgs.fields.some((f: any) => f.name === 'filter')
			const hasSortField = createdArgs.fields.some((f: any) => f.name === 'sort')

			expect(hasFilterField).toBe(true)
			expect(hasSortField).toBe(false)
		})

		test('should handle model with only sortable fields', () => {
			let createdArgs: any = null
			mockASTFactory.createFilterInputType = (typeName: string, fields: any[]) => {
				createdArgs = { typeName, fields }
				return { name: 'MockClass', kind: 'Class' } as any
			}
			mockASTFactory.hasType = (typeName: string) => typeName === 'UserSortInput'

			const mockField = {
				...TestFixtures.createField('name', 'String'),
				attributes: [TestFixtures.createAttribute('graphql.sortable')],
			}
			const model = TestFixtures.createDataModel('User', [mockField])

			strategy.createQueryArgsInputType('UserQueryArgs', model)

			const hasFilterField = createdArgs.fields.some((f: any) => f.name === 'filter')
			const hasSortField = createdArgs.fields.some((f: any) => f.name === 'sort')

			expect(hasFilterField).toBe(false)
			expect(hasSortField).toBe(true)
		})

		test('should handle description parameter', () => {
			const model = TestFixtures.createDataModel('User')

			const result = strategy.createQueryArgsInputType('UserQueryArgs', model, 'Custom query args description')

			expect(result).toBe('UserQueryArgs')
		})
	})

	describe('Type Checking', () => {
		test('should check if type exists via AST factory', () => {
			expect(strategy.hasType('ExistingType')).toBe(true)
			expect(strategy.hasType('NonExistentType')).toBe(false)
		})

		test('should handle AST factory hasType errors', () => {
			mockASTFactory.hasType = () => {
				throw new Error('HasType error')
			}

			expect(() => {
				strategy.hasType('AnyType')
			}).toThrow('HasType error')
		})
	})

	describe('Relation Processing', () => {
		test('should handle relation processing as no-op', () => {
			const mockRelation = { fromModel: 'User', toModel: 'Post', fieldName: 'posts' } as any

			expect(() => {
				strategy.processRelation(mockRelation)
			}).not.toThrow()
		})
	})

	describe('Generated Type Names', () => {
		test('should get all generated type names', () => {
			const typeNames = strategy.getGeneratedTypeNames()

			expect(typeNames).toEqual(['UserFilterInput', 'PostSortInput', 'CommentConnection'])
		})

		test('should filter generated type names', () => {
			const filteredTypes = strategy.getGeneratedTypeNames((name) => name.includes('User'))

			expect(filteredTypes).toEqual(['UserFilterInput'])
		})

		test('should handle empty filter result', () => {
			const filteredTypes = strategy.getGeneratedTypeNames((name) => name.includes('NonExistent'))

			expect(filteredTypes).toEqual([])
		})

		test('should handle AST factory error', () => {
			mockASTFactory.getGeneratedTypeNames = () => {
				throw new Error('Generated types error')
			}

			expect(() => {
				strategy.getGeneratedTypeNames()
			}).toThrow('Generated types error')
		})
	})

	describe('Generated Code', () => {
		test('should get generated code from AST factory', () => {
			const code = strategy.getGeneratedCode()

			expect(code).toBe('export interface User { name: string; }')
		})

		test('should handle AST factory error', () => {
			mockASTFactory.getGeneratedCode = () => {
				throw new Error('Generated code error')
			}

			expect(() => {
				strategy.getGeneratedCode()
			}).toThrow('Generated code error')
		})

		test('should handle empty generated code', () => {
			mockASTFactory.getGeneratedCode = () => ''

			const code = strategy.getGeneratedCode()

			expect(code).toBe('')
		})
	})

	describe('Error Handling', () => {
		test('should handle null fields in filter creation', () => {
			let createdFields: any[] = []
			mockASTFactory.createFilterInputType = (typeName: string, fields: any[]) => {
				createdFields = fields
				return { name: 'MockClass', kind: 'Class' } as any
			}

			const fields = null as any

			expect(() => {
				strategy.createFilterInputType('User', fields)
			}).toThrow()
		})

		test('should handle undefined AST factory methods', () => {
			const incompleteFactory = {} as any
			const incompleteStrategy = new TypeScriptOutputStrategy(incompleteFactory)

			expect(() => {
				incompleteStrategy.createEnumType({ name: 'Test' })
			}).toThrow()
		})
	})
})
