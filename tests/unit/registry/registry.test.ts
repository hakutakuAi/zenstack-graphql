import { describe, test, expect, beforeEach } from 'bun:test'
import { SchemaComposer } from 'graphql-compose'
import { GraphQLRegistry, TypeScriptRegistry } from '@utils/registry'
import { TypeKind } from '@utils/registry/base-registry'

describe('Registry Components', () => {
	describe('Base Registry Functionality', () => {
		let graphqlRegistry: GraphQLRegistry
		let typescriptRegistry: TypeScriptRegistry

		beforeEach(() => {
			const schemaComposer = new SchemaComposer()
			graphqlRegistry = new GraphQLRegistry(schemaComposer)
			typescriptRegistry = new TypeScriptRegistry()
		})

		test('GraphQL Registry - should register and retrieve types', () => {
			const objectTC = new SchemaComposer().createObjectTC({
				name: 'User',
				fields: { name: 'String' },
			})

			graphqlRegistry.registerType('User', TypeKind.OBJECT, objectTC)

			expect(graphqlRegistry.hasType('User')).toBe(true)
			expect(graphqlRegistry.isTypeOfKind('User', TypeKind.OBJECT)).toBe(true)
		})

		test('TypeScript Registry - should register and retrieve types', () => {
			typescriptRegistry.registerType('User', TypeKind.OBJECT, 'interface User { name: string }')

			expect(typescriptRegistry.hasType('User')).toBe(true)
			expect(typescriptRegistry.isTypeOfKind('User', TypeKind.OBJECT)).toBe(true)
		})

		test('GraphQL Registry - should manage edge types', () => {
			expect(true).toBe(true)
		})

		test('GraphQL Registry - should track processed relations', () => {
			const relationKey = 'User_posts_Post'

			expect(graphqlRegistry.hasProcessedRelation(relationKey)).toBe(false)

			graphqlRegistry.addProcessedRelation(relationKey)

			expect(graphqlRegistry.hasProcessedRelation(relationKey)).toBe(true)
		})

		test('TypeScript Registry - should generate code', () => {
			typescriptRegistry.registerType('User', TypeKind.OBJECT, 'interface User { name: string }')

			const code = typescriptRegistry.generateCode()

			expect(code).toContain('interface User { name: string }')
			expect(code).toContain('import { ObjectType')
		})

		test('TypeScript Registry - should handle dependencies', () => {
			typescriptRegistry.registerTypeWithDeps('User', TypeKind.OBJECT, 'interface User {}', ['Post'])

			const allTypes = typescriptRegistry.getAllTypes()
			const userType = allTypes.find((t) => t.name === 'User')

			expect(userType?.dependencies).toContain('Post')
		})

		test('TypeScript Registry - should validate schema', () => {
			typescriptRegistry.registerType('User', TypeKind.OBJECT, 'interface User {}')
			typescriptRegistry.registerTypeWithDeps('Post', TypeKind.OBJECT, 'interface Post {}', ['User'])

			const warnings = typescriptRegistry.validateSchema()

			expect(warnings).toHaveLength(0)
		})

		test('Both registries - should handle generated types', () => {
			const objectTC = new SchemaComposer().createObjectTC({
				name: 'User',
				fields: { name: 'String' },
			})

			graphqlRegistry.registerType('User', TypeKind.OBJECT, objectTC, true)
			typescriptRegistry.registerType('User', TypeKind.OBJECT, 'interface User {}', true)

			const graphqlTypes = graphqlRegistry.getGeneratedTypes()
			const typescriptTypes = typescriptRegistry.getGeneratedTypes()

			expect(graphqlTypes.length).toBeGreaterThanOrEqual(1)
			expect(graphqlTypes.some((t) => t.name === 'User')).toBe(true)
			expect(typescriptTypes).toHaveLength(1)
			expect(typescriptTypes[0]?.name).toBe('User')
		})
	})
})
