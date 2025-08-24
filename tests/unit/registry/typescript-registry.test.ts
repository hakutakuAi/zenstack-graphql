import { describe, test, expect, beforeEach } from 'bun:test'
import { TypeScriptRegistry } from '@utils/registry/typescript-registry'
import { TypeKind } from '@utils/registry/base-registry'

describe('TypeScript Registry', () => {
	let registry: TypeScriptRegistry

	beforeEach(() => {
		registry = new TypeScriptRegistry()
	})

	describe('Initialization', () => {
		test('should initialize with empty registry', () => {
			expect(registry).toBeDefined()
			expect(registry).toBeInstanceOf(TypeScriptRegistry)
			expect(registry.getAllTypes()).toHaveLength(0)
		})

		test('should initialize with default imports in generated code', () => {
			const code = registry.generateCode()
			expect(code).toContain('import { ObjectType, Field, ID')
		})
	})

	describe('Type Registration', () => {
		test('should register TypeScript types with string data', () => {
			registry.registerType('User', TypeKind.OBJECT, 'interface User { name: string }')

			expect(registry.hasType('User')).toBe(true)
			expect(registry.isTypeOfKind('User', TypeKind.OBJECT)).toBe(true)

			const allTypes = registry.getAllTypes()
			const userType = allTypes.find((t) => t.name === 'User')
			expect(userType?.data).toBe('interface User { name: string }')
		})

		test('should register enum types', () => {
			const enumCode = 'export enum UserRole { ADMIN = "ADMIN", USER = "USER" }'
			registry.registerType('UserRole', TypeKind.ENUM, enumCode)

			expect(registry.hasType('UserRole')).toBe(true)
			expect(registry.isTypeOfKind('UserRole', TypeKind.ENUM)).toBe(true)
		})

		test('should register input types', () => {
			const inputCode = 'export interface UserInput { name?: string; email: string }'
			registry.registerType('UserInput', TypeKind.INPUT, inputCode)

			expect(registry.hasType('UserInput')).toBe(true)
			expect(registry.isTypeOfKind('UserInput', TypeKind.INPUT)).toBe(true)
		})

		test('should register multiple types', () => {
			registry.registerType('User', TypeKind.OBJECT, 'interface User { name: string }')
			registry.registerType('Post', TypeKind.OBJECT, 'interface Post { title: string }')
			registry.registerType('UserRole', TypeKind.ENUM, 'enum UserRole { ADMIN, USER }')

			expect(registry.getAllTypes()).toHaveLength(3)
			expect(registry.getTypeNamesByKind(TypeKind.OBJECT)).toHaveLength(2)
			expect(registry.getTypeNamesByKind(TypeKind.ENUM)).toHaveLength(1)
		})

		test('should handle generated flag correctly', () => {
			registry.registerType('GeneratedType', TypeKind.OBJECT, 'interface GeneratedType {}', true)
			registry.registerType('ManualType', TypeKind.OBJECT, 'interface ManualType {}', false)

			const generatedTypes = registry.getGeneratedTypes()
			expect(generatedTypes).toHaveLength(1)
			expect(generatedTypes[0]?.name).toBe('GeneratedType')
		})
	})

	describe('Type Registration with Dependencies', () => {
		test('should register type with dependencies', () => {
			registry.registerTypeWithDeps('User', TypeKind.OBJECT, 'interface User { name: string }', ['Post'])

			expect(registry.hasType('User')).toBe(true)
			const allTypes = registry.getAllTypes()
			const userType = allTypes.find((t) => t.name === 'User')
			expect(userType?.dependencies).toContain('Post')
		})

		test('should not duplicate existing types in registerTypeWithDeps', () => {
			registry.registerTypeWithDeps('User', TypeKind.OBJECT, 'interface User { name: string }')
			registry.registerTypeWithDeps('User', TypeKind.OBJECT, 'interface User { name: string; age: number }')

			expect(registry.getAllTypes()).toHaveLength(1)
		})

		test('should allow kind change in registerTypeWithDeps if different', () => {
			registry.registerTypeWithDeps('FlexibleType', TypeKind.OBJECT, 'interface FlexibleType {}')
			registry.registerTypeWithDeps('FlexibleType', TypeKind.ENUM, 'enum FlexibleType { VALUE }')

			expect(registry.isTypeOfKind('FlexibleType', TypeKind.ENUM)).toBe(true)
		})
	})

	describe('Code Generation', () => {
		test('should generate TypeScript code from registered types', () => {
			const userCode = 'export interface User { name: string; age: number }'
			const roleCode = 'export enum UserRole { ADMIN = "ADMIN", USER = "USER" }'

			registry.registerType('User', TypeKind.OBJECT, userCode)
			registry.registerType('UserRole', TypeKind.ENUM, roleCode)

			const generatedCode = registry.generateCode()

			expect(generatedCode).toContain(userCode)
			expect(generatedCode).toContain(roleCode)
		})

		test('should include default imports', () => {
			const generatedCode = registry.generateCode()

			expect(generatedCode).toContain('import { ObjectType, Field, ID')
			expect(generatedCode).toContain('import { GraphQLJSON } from "graphql-scalars"')
			expect(generatedCode).toContain('import "reflect-metadata"')
		})

		test('should only include generated types in code', () => {
			registry.registerType('GeneratedType', TypeKind.OBJECT, 'interface GeneratedType {}', true)
			registry.registerType('ManualType', TypeKind.OBJECT, 'interface ManualType {}', false)

			const generatedCode = registry.generateCode()

			expect(generatedCode).toContain('GeneratedType')
			expect(generatedCode).not.toContain('ManualType')
		})

		test('should handle empty registry', () => {
			const generatedCode = registry.generateCode()

			expect(generatedCode).toContain('import { ObjectType')
			expect(generatedCode.split('\n')).toHaveLength(4)
		})
	})

	describe('Type Retrieval by Kind', () => {
		beforeEach(() => {
			registry.registerType('User', TypeKind.OBJECT, '@ObjectType() class User { name: string }')
			registry.registerType('UserRole', TypeKind.ENUM, 'enum UserRole { ADMIN, USER }')
			registry.registerType('UserInput', TypeKind.INPUT, 'class UserInput { name: string }')
			registry.registerType('DateTime', TypeKind.SCALAR, 'const DateTime = GraphQLDateTime')
		})

		test('should get object types', () => {
			const objectTypes = registry.getObjectTypes()

			expect(objectTypes).toHaveLength(1)
			expect(objectTypes[0]).toContain('class User')
		})

		test('should get enum types', () => {
			const enumTypes = registry.getEnumTypes()

			expect(enumTypes).toHaveLength(1)
			expect(enumTypes[0]).toContain('enum UserRole')
		})

		test('should get input types', () => {
			const inputTypes = registry.getInputTypes()

			expect(inputTypes).toHaveLength(1)
			expect(inputTypes[0]).toContain('class UserInput')
		})

		test('should get scalar types', () => {
			const scalarTypes = registry.getScalarTypes()

			expect(scalarTypes).toHaveLength(1)
			expect(scalarTypes[0]).toContain('DateTime')
		})

		test('should get resolver types', () => {
			registry.registerType('UserResolver', TypeKind.RESOLVER, 'class UserResolver {}')
			const resolverTypes = registry.getResolverTypes()

			expect(resolverTypes).toHaveLength(1)
			expect(resolverTypes[0]).toContain('UserResolver')
		})
	})

	describe('Dependency Management', () => {
		test('should add dependencies to existing types', () => {
			registry.registerType('User', TypeKind.OBJECT, 'interface User { name: string }')
			registry.addDependency('User', 'Post')

			const allTypes = registry.getAllTypes()
			const userType = allTypes.find((t) => t.name === 'User')
			expect(userType?.dependencies).toContain('Post')
		})

		test('should not duplicate dependencies', () => {
			registry.registerType('User', TypeKind.OBJECT, 'interface User { name: string }')
			registry.addDependency('User', 'Post')
			registry.addDependency('User', 'Post')

			const allTypes = registry.getAllTypes()
			const userType = allTypes.find((t) => t.name === 'User')
			const postDependencies = userType?.dependencies.filter((d) => d === 'Post')
			expect(postDependencies).toHaveLength(1)
		})

		test('should handle adding dependency to non-existent type', () => {
			expect(() => {
				registry.addDependency('NonExistent', 'SomeDep')
			}).not.toThrow()
		})

		test('should get dependency graph', () => {
			registry.registerTypeWithDeps('User', TypeKind.OBJECT, 'interface User {}', ['Post'])
			registry.registerTypeWithDeps('Post', TypeKind.OBJECT, 'interface Post {}', ['Comment'])
			registry.registerTypeWithDeps('Comment', TypeKind.OBJECT, 'interface Comment {}', [])

			const dependencyGraph = registry.getDependencyGraph()

			expect(dependencyGraph.get('User')).toEqual(['Post'])
			expect(dependencyGraph.get('Post')).toEqual(['Comment'])
			expect(dependencyGraph.get('Comment')).toEqual([])
		})

		test('should handle circular dependencies in graph', () => {
			registry.registerTypeWithDeps('A', TypeKind.OBJECT, 'interface A {}', ['B'])
			registry.registerTypeWithDeps('B', TypeKind.OBJECT, 'interface B {}', ['A'])

			const dependencyGraph = registry.getDependencyGraph()

			expect(dependencyGraph.get('A')).toEqual(['B'])
			expect(dependencyGraph.get('B')).toEqual(['A'])
		})
	})

	describe('Schema Validation', () => {
		test('should return no warnings for valid dependencies', () => {
			registry.registerType('User', TypeKind.OBJECT, 'interface User { name: string }')
			registry.registerTypeWithDeps('Post', TypeKind.OBJECT, 'interface Post {}', ['User'])

			const warnings = registry.validateSchema()

			expect(warnings).toHaveLength(0)
		})

		test('should return warnings for missing dependencies', () => {
			registry.registerTypeWithDeps('Post', TypeKind.OBJECT, 'interface Post {}', ['User', 'Category'])

			const warnings = registry.validateSchema()

			expect(warnings).toHaveLength(2)
			expect(warnings[0]).toContain('Post depends on User which is not registered')
			expect(warnings[1]).toContain('Post depends on Category which is not registered')
		})

		test('should handle multiple types with missing dependencies', () => {
			registry.registerTypeWithDeps('Post', TypeKind.OBJECT, 'interface Post {}', ['User'])
			registry.registerTypeWithDeps('Comment', TypeKind.OBJECT, 'interface Comment {}', ['User', 'Post'])

			const warnings = registry.validateSchema()

			expect(warnings).toHaveLength(2)
		})

		test('should handle types with no dependencies', () => {
			registry.registerType('User', TypeKind.OBJECT, 'interface User { name: string }')

			const warnings = registry.validateSchema()

			expect(warnings).toHaveLength(0)
		})
	})

	describe('Import Generation', () => {
		test('should generate default imports', () => {
			const imports = registry.generateImports()

			expect(imports).toContain('import { ObjectType, Field, ID, Int, Float, registerEnumType, InputType, ArgsType } from "type-graphql"')
			expect(imports).toContain('import { GraphQLJSON } from "graphql-scalars"')
			expect(imports).toContain('import "reflect-metadata"')
		})

		test('should handle custom scalars in imports', () => {
			registry.registerType('CustomScalar', TypeKind.SCALAR, 'const CustomScalar = GraphQLCustom')

			const imports = registry.generateImports()

			expect(imports.length).toBeGreaterThanOrEqual(3)
		})

		test('should handle no custom scalars', () => {
			const imports = registry.generateImports()

			expect(imports).toHaveLength(3)
		})
	})

	describe('Integration with Base Registry', () => {
		test('should inherit all base registry functionality', () => {
			registry.registerType('User', TypeKind.OBJECT, 'interface User {}')

			expect(registry.hasType('User')).toBe(true)
			expect(registry.getAllTypes()).toHaveLength(1)
			expect(registry.getTypeNamesByKind(TypeKind.OBJECT)).toContain('User')
			expect(registry.isTypeOfKind('User', TypeKind.OBJECT)).toBe(true)
		})

		test('should support generated type filtering from base', () => {
			registry.registerType('GeneratedType', TypeKind.OBJECT, 'interface GeneratedType {}', true)
			registry.registerType('ManualType', TypeKind.OBJECT, 'interface ManualType {}', false)

			const generatedTypes = registry.getGeneratedTypes()
			expect(generatedTypes).toHaveLength(1)
			expect(generatedTypes[0]?.name).toBe('GeneratedType')
		})
	})

	describe('Edge Cases', () => {
		test('should handle empty type names', () => {
			registry.registerType('', TypeKind.OBJECT, 'interface Empty {}')

			expect(registry.hasType('')).toBe(true)
		})

		test('should handle empty type code', () => {
			registry.registerType('EmptyType', TypeKind.OBJECT, '')

			expect(registry.hasType('EmptyType')).toBe(true)
			const allTypes = registry.getAllTypes()
			const emptyType = allTypes.find((t) => t.name === 'EmptyType')
			expect(emptyType?.data).toBe('')
		})

		test('should handle null type code', () => {
			registry.registerType('NullType', TypeKind.OBJECT, null as any)

			expect(registry.hasType('NullType')).toBe(true)
		})

		test('should handle special characters in type names', () => {
			const specialName = 'Type_With-Special.Characters123'
			registry.registerType(specialName, TypeKind.OBJECT, 'interface SpecialType {}')

			expect(registry.hasType(specialName)).toBe(true)
		})
	})

	describe('Performance', () => {
		test('should handle large numbers of types efficiently', () => {
			const startTime = Date.now()

			for (let i = 0; i < 1000; i++) {
				registry.registerType(`Type${i}`, TypeKind.OBJECT, `interface Type${i} { id: string }`)
			}

			const endTime = Date.now()
			const duration = endTime - startTime

			expect(duration).toBeLessThan(1000)
			expect(registry.getAllTypes()).toHaveLength(1000)
		})

		test('should generate large code efficiently', () => {
			for (let i = 0; i < 100; i++) {
				registry.registerType(`Type${i}`, TypeKind.OBJECT, `interface Type${i} { id: string }`)
			}

			const startTime = Date.now()
			const generatedCode = registry.generateCode()
			const endTime = Date.now()

			expect(endTime - startTime).toBeLessThan(100)
			expect(generatedCode.length).toBeGreaterThan(0)
		})

		test('should handle dependency operations efficiently', () => {
			for (let i = 0; i < 100; i++) {
				const deps = i > 0 ? [`Type${i - 1}`] : []
				registry.registerTypeWithDeps(`Type${i}`, TypeKind.OBJECT, `interface Type${i} {}`, deps)
			}

			const startTime = Date.now()
			const warnings = registry.validateSchema()
			const endTime = Date.now()

			expect(endTime - startTime).toBeLessThan(100)
			expect(warnings).toHaveLength(0)
		})
	})
})
