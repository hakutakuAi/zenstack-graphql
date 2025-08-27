import { describe, it, expect, beforeEach } from 'bun:test'
import { SchemaComposer } from 'graphql-compose'
import { GraphQLRegistry } from '@utils/registry/graphql-registry'
import { TypeKind } from '@utils/registry/base-registry'

describe('GraphQL Registry', () => {
	let registry: GraphQLRegistry
	let schemaComposer: SchemaComposer<unknown>

	beforeEach(() => {
		schemaComposer = new SchemaComposer()
		registry = new GraphQLRegistry(schemaComposer)
	})

	describe('Initialization', () => {
		it('should initialize with schema composer', () => {
			expect(registry).toBeDefined()
			expect(registry).toBeInstanceOf(GraphQLRegistry)
		})

		it('should create Node interface on initialization', () => {
			expect(registry.hasType('Node')).toBe(true)
			expect(registry.isTypeOfKind('Node', TypeKind.INTERFACE)).toBe(true)
		})

		it('should create Edge interface on initialization', () => {
			expect(registry.hasType('Edge')).toBe(true)
			expect(registry.isTypeOfKind('Edge', TypeKind.INTERFACE)).toBe(true)
		})

		it('should create Connection interface on initialization', () => {
			expect(registry.hasType('Connection')).toBe(true)
			expect(registry.isTypeOfKind('Connection', TypeKind.INTERFACE)).toBe(true)
		})

		it('should sync existing types from schema composer', () => {
			const composerWithTypes = new SchemaComposer()
			composerWithTypes.createObjectTC('ExistingType')

			const registryWithTypes = new GraphQLRegistry(composerWithTypes)

			expect(registryWithTypes.hasType('ExistingType')).toBe(true)
			expect(registryWithTypes.isTypeOfKind('ExistingType', TypeKind.OBJECT)).toBe(true)
		})
	})

	describe('Type Registration', () => {
		it('should register GraphQL object type', () => {
			const objectTC = schemaComposer.createObjectTC({
				name: 'User',
				fields: { name: 'String' },
			})

			registry.registerType('User', TypeKind.OBJECT, objectTC)

			expect(registry.hasType('User')).toBe(true)
			expect(registry.isTypeOfKind('User', TypeKind.OBJECT)).toBe(true)
		})

		it('should register GraphQL input type', () => {
			const inputTC = schemaComposer.createInputTC({
				name: 'UserInput',
				fields: { name: 'String' },
			})

			registry.registerType('UserInput', TypeKind.INPUT, inputTC)

			expect(registry.hasType('UserInput')).toBe(true)
			expect(registry.isTypeOfKind('UserInput', TypeKind.INPUT)).toBe(true)
		})

		it('should register GraphQL enum type', () => {
			const enumTC = schemaComposer.createEnumTC({
				name: 'UserRole',
				values: { ADMIN: { value: 'ADMIN' }, USER: { value: 'USER' } },
			})

			registry.registerType('UserRole', TypeKind.ENUM, enumTC)

			expect(registry.hasType('UserRole')).toBe(true)
			expect(registry.isTypeOfKind('UserRole', TypeKind.ENUM)).toBe(true)
		})

		it('should register GraphQL scalar type', () => {
			const scalarTC = schemaComposer.createScalarTC({
				name: 'DateTime',
				serialize: (value: any) => value,
			})

			registry.registerType('DateTime', TypeKind.SCALAR, scalarTC)

			expect(registry.hasType('DateTime')).toBe(true)
			expect(registry.isTypeOfKind('DateTime', TypeKind.SCALAR)).toBe(true)
		})

		it('should store composer reference in type info', () => {
			const objectTC = schemaComposer.createObjectTC({
				name: 'User',
				fields: { name: 'String' },
			})

			registry.registerType('User', TypeKind.OBJECT, objectTC)

			const typeInfo = registry.getType('User')
			expect(typeInfo?.composer).toBe(objectTC)
		})
	})

	describe('Edge Type Management', () => {
		it('should register edge type', () => {
			const userEdgeTC = schemaComposer.createObjectTC({ name: 'UserEdge', fields: { cursor: 'String!' } })
			registry.registerEdgeType('UserEdge', userEdgeTC)

			const edgeTypes = registry.getEdgeTypes()
			expect(edgeTypes).toContain('UserEdge')
		})

		it('should not duplicate edge types', () => {
			const userEdgeTC = schemaComposer.createObjectTC({ name: 'UserEdge', fields: { cursor: 'String!' } })
			registry.registerEdgeType('UserEdge', userEdgeTC)
			registry.registerEdgeType('UserEdge', userEdgeTC)

			const edgeTypes = registry.getEdgeTypes()
			expect(edgeTypes.filter((e) => e === 'UserEdge')).toHaveLength(1)
		})

		it('should register multiple edge types', () => {
			const userEdgeTC = schemaComposer.createObjectTC({ name: 'UserEdge', fields: { cursor: 'String!' } })
			const postEdgeTC = schemaComposer.createObjectTC({ name: 'PostEdge', fields: { cursor: 'String!' } })
			registry.registerEdgeType('UserEdge', userEdgeTC)
			registry.registerEdgeType('PostEdge', postEdgeTC)

			const edgeTypes = registry.getEdgeTypes()
			expect(edgeTypes).toContain('UserEdge')
			expect(edgeTypes).toContain('PostEdge')
			expect(edgeTypes).toHaveLength(2)
		})

		it('should get all edge types', () => {
			const userEdgeTC = schemaComposer.createObjectTC({ name: 'UserEdge', fields: { cursor: 'String!' } })
			const postEdgeTC = schemaComposer.createObjectTC({ name: 'PostEdge', fields: { cursor: 'String!' } })
			const commentEdgeTC = schemaComposer.createObjectTC({ name: 'CommentEdge', fields: { cursor: 'String!' } })
			registry.registerEdgeType('UserEdge', userEdgeTC)
			registry.registerEdgeType('PostEdge', postEdgeTC)
			registry.registerEdgeType('CommentEdge', commentEdgeTC)

			const edgeTypes = registry.getEdgeTypes()
			expect(edgeTypes.sort()).toEqual(['CommentEdge', 'PostEdge', 'UserEdge'])
		})
	})

	describe('Relation Processing', () => {
		it('should track processed relations', () => {
			const relationKey = 'User_posts_Post'

			expect(registry.hasProcessedRelation(relationKey)).toBe(false)

			registry.addProcessedRelation(relationKey)

			expect(registry.hasProcessedRelation(relationKey)).toBe(true)
		})

		it('should not duplicate processed relations', () => {
			const relationKey = 'User_posts_Post'

			registry.addProcessedRelation(relationKey)
			registry.addProcessedRelation(relationKey)

			const processedRelations = registry.getProcessedRelations()
			expect(processedRelations.filter((r) => r === relationKey)).toHaveLength(1)
		})

		it('should get all processed relations', () => {
			const relation1 = 'User_posts_Post'
			const relation2 = 'Post_author_User'

			registry.addProcessedRelation(relation1)
			registry.addProcessedRelation(relation2)

			const processedRelations = registry.getProcessedRelations()
			expect(processedRelations.sort()).toEqual([relation1, relation2].sort())
		})

		it('should handle empty relation keys', () => {
			registry.addProcessedRelation('')

			expect(registry.hasProcessedRelation('')).toBe(true)
			expect(registry.getProcessedRelations()).toContain('')
		})
	})

	describe('Built-in Scalar Detection', () => {
		it('should detect GraphQL built-in scalars', () => {
			expect(registry.isBuiltInScalar('String')).toBe(true)
			expect(registry.isBuiltInScalar('Int')).toBe(true)
			expect(registry.isBuiltInScalar('Float')).toBe(true)
			expect(registry.isBuiltInScalar('Boolean')).toBe(true)
			expect(registry.isBuiltInScalar('ID')).toBe(true)
		})

		it('should not detect custom scalars as built-in', () => {
			expect(registry.isBuiltInScalar('DateTime')).toBe(false)
			expect(registry.isBuiltInScalar('JSON')).toBe(false)
			expect(registry.isBuiltInScalar('UUID')).toBe(false)
		})

		it('should handle case sensitivity', () => {
			expect(registry.isBuiltInScalar('string')).toBe(false)
			expect(registry.isBuiltInScalar('STRING')).toBe(false)
			expect(registry.isBuiltInScalar('int')).toBe(false)
		})

		it('should handle null and undefined inputs', () => {
			expect(registry.isBuiltInScalar(null as any)).toBe(false)
			expect(registry.isBuiltInScalar(undefined as any)).toBe(false)
		})
	})

	describe('Schema Generation', () => {
		it('should generate GraphQL SDL schema', () => {
			const pageInfoTC = schemaComposer.createObjectTC({
				name: 'PageInfo',
				fields: {
					hasNextPage: 'Boolean!',
					hasPreviousPage: 'Boolean!',
					startCursor: 'String',
					endCursor: 'String',
				},
			})
			registry.registerType('PageInfo', TypeKind.OBJECT, pageInfoTC)

			const objectTC = schemaComposer.createObjectTC({
				name: 'User',
				fields: { name: 'String', age: 'Int' },
			})
			registry.registerType('User', TypeKind.OBJECT, objectTC)

			const schema = registry.generateSDL()

			expect(schema).toContain('type User')
			expect(schema).toContain('name: String')
			expect(schema).toContain('age: Int')
		})

		it('should include Node interface in schema', () => {
			const pageInfoTC = schemaComposer.createObjectTC({
				name: 'PageInfo',
				fields: {
					hasNextPage: 'Boolean!',
					hasPreviousPage: 'Boolean!',
					startCursor: 'String',
					endCursor: 'String',
				},
			})
			registry.registerType('PageInfo', TypeKind.OBJECT, pageInfoTC)

			const schema = registry.generateSDL()

			expect(schema).toContain('interface Node')
			expect(schema).toContain('id: ID!')
		})

		it('should include Edge interface in schema', () => {
			const pageInfoTC = schemaComposer.createObjectTC({
				name: 'PageInfo',
				fields: {
					hasNextPage: 'Boolean!',
					hasPreviousPage: 'Boolean!',
					startCursor: 'String',
					endCursor: 'String',
				},
			})
			registry.registerType('PageInfo', TypeKind.OBJECT, pageInfoTC)

			const schema = registry.generateSDL()

			expect(schema).toContain('interface Edge')
			expect(schema).toContain('cursor: String!')
		})

		it('should include Connection interface in schema', () => {
			const pageInfoTC = schemaComposer.createObjectTC({
				name: 'PageInfo',
				fields: {
					hasNextPage: 'Boolean!',
					hasPreviousPage: 'Boolean!',
					startCursor: 'String',
					endCursor: 'String',
				},
			})
			registry.registerType('PageInfo', TypeKind.OBJECT, pageInfoTC)

			const schema = registry.generateSDL()

			expect(schema).toContain('interface Connection')
			expect(schema).toContain('pageInfo: PageInfo!')
			expect(schema).toContain('totalCount: Int!')
		})

		it('should handle empty schema', () => {
			const emptyComposer = new SchemaComposer()
			const emptyRegistry = new GraphQLRegistry(emptyComposer)

			const pageInfoTC = emptyComposer.createObjectTC({
				name: 'PageInfo',
				fields: {
					hasNextPage: 'Boolean!',
					hasPreviousPage: 'Boolean!',
					startCursor: 'String',
					endCursor: 'String',
				},
			})
			emptyRegistry.registerType('PageInfo', TypeKind.OBJECT, pageInfoTC)

			const schema = emptyRegistry.generateSDL()

			expect(schema).toBeDefined()
			expect(typeof schema).toBe('string')
		})
	})

	describe('Type Validation', () => {
		it('should validate registered types exist in schema composer', () => {
			const objectTC = schemaComposer.createObjectTC({
				name: 'User',
				fields: { name: 'String' },
			})
			registry.registerType('User', TypeKind.OBJECT, objectTC)

			expect(schemaComposer.has('User')).toBe(true)
		})

		it('should handle missing types gracefully', () => {
			const fakeTC = { getTypeName: () => 'FakeType' } as any
			registry.registerType('FakeType', TypeKind.OBJECT, fakeTC)

			expect(registry.hasType('FakeType')).toBe(true)
			expect(schemaComposer.has('FakeType')).toBe(false)
		})
	})

	describe('Edge and Connection Type Implementation', () => {
		it('should create edge types that implement Edge interface', () => {
			const pageInfoTC = schemaComposer.createObjectTC({
				name: 'PageInfo',
				fields: {
					hasNextPage: 'Boolean!',
					hasPreviousPage: 'Boolean!',
					startCursor: 'String',
					endCursor: 'String',
				},
			})
			registry.registerType('PageInfo', TypeKind.OBJECT, pageInfoTC)

			const userTC = schemaComposer.createObjectTC({
				name: 'User',
				fields: { name: 'String' },
			})
			registry.registerType('User', TypeKind.OBJECT, userTC)

			const edgeTC = schemaComposer.createObjectTC({
				name: 'UserEdge',
				interfaces: ['Edge'],
				fields: {
					node: 'User!',
					cursor: 'String!',
				},
			})

			registry.registerEdgeType('UserEdge', edgeTC)

			expect(registry.hasEdgeType('UserEdge')).toBe(true)
			expect(registry.isTypeOfKind('UserEdge', TypeKind.EDGE)).toBe(true)

			const schema = registry.generateSDL()
			expect(schema).toContain('type UserEdge implements Edge')
			expect(schema).toContain('node: User!')
			expect(schema).toContain('cursor: String!')
		})

		it('should create connection types that implement Connection interface', () => {
			const pageInfoTC = schemaComposer.createObjectTC({
				name: 'PageInfo',
				fields: {
					hasNextPage: 'Boolean!',
					hasPreviousPage: 'Boolean!',
					startCursor: 'String',
					endCursor: 'String',
				},
			})
			registry.registerType('PageInfo', TypeKind.OBJECT, pageInfoTC)

			const userTC = schemaComposer.createObjectTC({
				name: 'User',
				fields: { name: 'String' },
			})
			registry.registerType('User', TypeKind.OBJECT, userTC)

			const userEdgeTC = schemaComposer.createObjectTC({
				name: 'UserEdge',
				interfaces: ['Edge'],
				fields: {
					node: 'User!',
					cursor: 'String!',
				},
			})
			registry.registerEdgeType('UserEdge', userEdgeTC)

			const connectionTC = schemaComposer.createObjectTC({
				name: 'UserConnection',
				interfaces: ['Connection'],
				fields: {
					pageInfo: 'PageInfo!',
					edges: '[UserEdge!]!',
					totalCount: 'Int!',
				},
			})

			registry.registerType('UserConnection', TypeKind.CONNECTION, connectionTC)

			expect(registry.hasType('UserConnection')).toBe(true)
			expect(registry.isTypeOfKind('UserConnection', TypeKind.CONNECTION)).toBe(true)

			const schema = registry.generateSDL()
			expect(schema).toContain('type UserConnection implements Connection')
			expect(schema).toContain('pageInfo: PageInfo!')
			expect(schema).toContain('edges: [UserEdge!]!')
			expect(schema).toContain('totalCount: Int!')
		})

		it('should maintain edge type registry', () => {
			const edgeTC = schemaComposer.createObjectTC({
				name: 'BookEdge',
				interfaces: ['Edge'],
				fields: { node: 'Book!', cursor: 'String!' },
			})

			registry.registerEdgeType('BookEdge', edgeTC)

			expect(registry.getEdgeTypes()).toContain('BookEdge')
			expect(registry.hasEdgeType('BookEdge')).toBe(true)
		})
	})

	describe('Type Description Extraction', () => {
		it('should extract description from object type composer', () => {
			const objectTC = schemaComposer.createObjectTC({
				name: 'User',
				description: 'User object type',
				fields: { name: 'String' },
			})
			registry.registerType('User', TypeKind.OBJECT, objectTC)

			const typeInfo = registry.getType('User')
			expect(typeInfo?.description).toBe('User object type')
		})

		it('should extract description from enum type composer', () => {
			const enumTC = schemaComposer.createEnumTC({
				name: 'UserRole',
				description: 'User role enumeration',
				values: { ADMIN: { value: 'ADMIN' } },
			})
			registry.registerType('UserRole', TypeKind.ENUM, enumTC)

			const typeInfo = registry.getType('UserRole')
			expect(typeInfo?.description).toBe('User role enumeration')
		})

		it('should handle types without description', () => {
			const objectTC = schemaComposer.createObjectTC({
				name: 'User',
				fields: { name: 'String' },
			})
			registry.registerType('User', TypeKind.OBJECT, objectTC)

			const typeInfo = registry.getType('User')
			expect(typeInfo?.description).toBeUndefined()
		})
	})

	describe('Integration with Base Registry', () => {
		it('should inherit all base registry functionality', () => {
			const objectTC = schemaComposer.createObjectTC({
				name: 'User',
				fields: { name: 'String' },
			})
			registry.registerType('User', TypeKind.OBJECT, objectTC)

			expect(registry.hasType('User')).toBe(true)
			expect(registry.getAllTypes().length).toBeGreaterThan(0)
			expect(registry.getTypeNamesByKind(TypeKind.OBJECT)).toContain('User')
			expect(registry.isTypeOfKind('User', TypeKind.OBJECT)).toBe(true)
		})

		it('should support generated type filtering', () => {
			const objectTC = schemaComposer.createObjectTC({
				name: 'GeneratedUser',
				fields: { name: 'String' },
			})
			registry.registerType('GeneratedUser', TypeKind.OBJECT, objectTC, true)

			const generatedTypes = registry.getGeneratedTypes()
			expect(generatedTypes.some((t) => t.name === 'GeneratedUser')).toBe(true)
		})
	})

	describe('Error Handling', () => {
		it('should handle invalid composer objects', () => {
			const invalidTC = null as any

			expect(() => {
				registry.registerType('InvalidType', TypeKind.OBJECT, invalidTC)
			}).not.toThrow()

			expect(registry.hasType('InvalidType')).toBe(true)
		})

		it('should handle type registration with empty names', () => {
			const objectTC = schemaComposer.createObjectTC({
				name: 'EmptyName',
				fields: { name: 'String' },
			})

			expect(() => {
				registry.registerType('', TypeKind.OBJECT, objectTC)
			}).not.toThrow()

			expect(registry.hasType('')).toBe(true)
		})
	})

	describe('Performance Considerations', () => {
		it('should handle large numbers of type registrations efficiently', () => {
			const startTime = Date.now()

			for (let i = 0; i < 1000; i++) {
				const objectTC = schemaComposer.createObjectTC({
					name: `Type${i}`,
					fields: { id: 'ID' },
				})
				registry.registerType(`Type${i}`, TypeKind.OBJECT, objectTC)
			}

			const endTime = Date.now()
			const duration = endTime - startTime

			expect(duration).toBeLessThan(1000)
			expect(registry.getAllTypes()).toHaveLength(1003)
		})

		it('should handle many edge type registrations efficiently', () => {
			for (let i = 0; i < 100; i++) {
				const edgeTC = schemaComposer.createObjectTC({ name: `Edge${i}`, fields: { cursor: 'String!' } })
				registry.registerEdgeType(`Edge${i}`, edgeTC)
			}

			expect(registry.getEdgeTypes()).toHaveLength(100)
		})

		it('should handle many relation processing operations efficiently', () => {
			for (let i = 0; i < 100; i++) {
				registry.addProcessedRelation(`Relation${i}`)
			}

			expect(registry.getProcessedRelations()).toHaveLength(100)
		})
	})
})
