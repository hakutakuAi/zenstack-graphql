import { describe, it, expect, beforeEach } from 'bun:test'
import { BaseRegistry, TypeKind, BaseTypeInfo } from '@utils/registry/base-registry'

interface TestTypeInfo extends BaseTypeInfo<string> {
	testData: string
}

class TestRegistry extends BaseRegistry<string, TestTypeInfo> {
	protected createTypeInfo(name: string, kind: TypeKind, data: string, isGenerated: boolean): TestTypeInfo {
		return {
			name,
			kind,
			description: `Test type ${name}`,
			data,
			isGenerated,
			testData: `Test data for ${name}`,
		}
	}

	validateSchema(): string[] {
		const warnings: string[] = []
		const types = this.getAllTypes()

		for (const typeInfo of types) {
			if (typeInfo.dependencies) {
				for (const dep of typeInfo.dependencies) {
					if (!this.hasType(dep)) {
						warnings.push(`${typeInfo.name} depends on ${dep} which is not registered`)
					}
				}
			}
		}

		return warnings
	}
}

describe('Base Registry', () => {
	let registry: TestRegistry

	beforeEach(() => {
		registry = new TestRegistry()
	})

	describe('Initialization', () => {
		it('should initialize with empty registry', () => {
			expect(registry).toBeDefined()
			expect(registry.getAllTypes()).toHaveLength(0)
		})

		it('should have empty type names by kind initially', () => {
			expect(registry.getTypeNamesByKind(TypeKind.OBJECT)).toHaveLength(0)
			expect(registry.getTypeNamesByKind(TypeKind.ENUM)).toHaveLength(0)
			expect(registry.getTypeNamesByKind(TypeKind.SCALAR)).toHaveLength(0)
		})
	})

	describe('Type Registration', () => {
		it('should register a new type', () => {
			registry.registerType('User', TypeKind.OBJECT, 'user-data')

			expect(registry.hasType('User')).toBe(true)
			expect(registry.getType('User')).toBeDefined()
			expect(registry.getType('User')?.name).toBe('User')
			expect(registry.getType('User')?.kind).toBe(TypeKind.OBJECT)
		})

		it('should register multiple types', () => {
			registry.registerType('User', TypeKind.OBJECT, 'user-data')
			registry.registerType('Post', TypeKind.OBJECT, 'post-data')
			registry.registerType('UserRole', TypeKind.ENUM, 'role-data')

			expect(registry.hasType('User')).toBe(true)
			expect(registry.hasType('Post')).toBe(true)
			expect(registry.hasType('UserRole')).toBe(true)
			expect(registry.getAllTypes()).toHaveLength(3)
		})

		it('should mark generated types correctly', () => {
			registry.registerType('GeneratedType', TypeKind.OBJECT, 'data', true)
			registry.registerType('ManualType', TypeKind.OBJECT, 'data', false)

			const generatedType = registry.getType('GeneratedType')
			const manualType = registry.getType('ManualType')

			expect(generatedType?.isGenerated).toBe(true)
			expect(manualType?.isGenerated).toBe(false)
		})

		it('should update existing type when re-registered', () => {
			registry.registerType('User', TypeKind.OBJECT, 'original-data')
			registry.registerType('User', TypeKind.OBJECT, 'updated-data')

			expect(registry.getType('User')?.data).toBe('updated-data')
			expect(registry.getAllTypes()).toHaveLength(1)
		})

		it('should handle empty type name', () => {
			registry.registerType('', TypeKind.OBJECT, 'empty-name-data')

			expect(registry.hasType('')).toBe(true)
			expect(registry.getType('')?.name).toBe('')
		})
	})

	describe('Type Retrieval', () => {
		beforeEach(() => {
			registry.registerType('User', TypeKind.OBJECT, 'user-data')
			registry.registerType('Post', TypeKind.OBJECT, 'post-data')
			registry.registerType('UserRole', TypeKind.ENUM, 'role-data')
			registry.registerType('ID', TypeKind.SCALAR, 'id-data')
		})

		it('should check type existence', () => {
			expect(registry.hasType('User')).toBe(true)
			expect(registry.hasType('NonExistent')).toBe(false)
		})

		it('should get type info', () => {
			const userInfo = registry.getType('User')

			expect(userInfo).toBeDefined()
			expect(userInfo?.name).toBe('User')
			expect(userInfo?.kind).toBe(TypeKind.OBJECT)
			expect(userInfo?.data).toBe('user-data')
			expect(userInfo?.testData).toBe('Test data for User')
		})

		it('should return undefined for non-existent type', () => {
			const nonExistentInfo = registry.getType('NonExistent')

			expect(nonExistentInfo).toBeUndefined()
		})

		it('should get all types', () => {
			const allTypes = registry.getAllTypes()

			expect(allTypes).toHaveLength(4)
			expect(allTypes.map((t) => t.name).sort()).toEqual(['ID', 'Post', 'User', 'UserRole'])
		})

		it('should get type names by kind', () => {
			const objectTypes = registry.getTypeNamesByKind(TypeKind.OBJECT)
			const enumTypes = registry.getTypeNamesByKind(TypeKind.ENUM)
			const scalarTypes = registry.getTypeNamesByKind(TypeKind.SCALAR)

			expect(objectTypes.sort()).toEqual(['Post', 'User'])
			expect(enumTypes).toEqual(['UserRole'])
			expect(scalarTypes).toEqual(['ID'])
		})

		it('should return empty array for kinds with no types', () => {
			const interfaceTypes = registry.getTypeNamesByKind(TypeKind.INTERFACE)
			const unionTypes = registry.getTypeNamesByKind(TypeKind.UNION)

			expect(interfaceTypes).toEqual([])
			expect(unionTypes).toEqual([])
		})
	})

	describe('Type Kind Checking', () => {
		beforeEach(() => {
			registry.registerType('User', TypeKind.OBJECT, 'user-data')
			registry.registerType('UserRole', TypeKind.ENUM, 'role-data')
		})

		it('should check if type is of specific kind', () => {
			expect(registry.isTypeOfKind('User', TypeKind.OBJECT)).toBe(true)
			expect(registry.isTypeOfKind('User', TypeKind.ENUM)).toBe(false)
			expect(registry.isTypeOfKind('UserRole', TypeKind.ENUM)).toBe(true)
			expect(registry.isTypeOfKind('UserRole', TypeKind.OBJECT)).toBe(false)
		})

		it('should return false for non-existent types', () => {
			expect(registry.isTypeOfKind('NonExistent', TypeKind.OBJECT)).toBe(false)
			expect(registry.isTypeOfKind('NonExistent', TypeKind.ENUM)).toBe(false)
		})
	})

	describe('Generated Types Filtering', () => {
		beforeEach(() => {
			registry.registerType('GeneratedUser', TypeKind.OBJECT, 'user-data', true)
			registry.registerType('ManualPost', TypeKind.OBJECT, 'post-data', false)
			registry.registerType('GeneratedEnum', TypeKind.ENUM, 'enum-data', true)
		})

		it('should get only generated types', () => {
			const generatedTypes = registry.getGeneratedTypes()

			expect(generatedTypes).toHaveLength(2)
			expect(generatedTypes.map((t) => t.name).sort()).toEqual(['GeneratedEnum', 'GeneratedUser'])
		})

		it('should get generated type names by kind', () => {
			const generatedTypes = registry.getGeneratedTypes()
			const generatedObjectTypes = generatedTypes.filter((t) => t.kind === TypeKind.OBJECT)
			const generatedEnumTypes = generatedTypes.filter((t) => t.kind === TypeKind.ENUM)

			expect(generatedObjectTypes.map((t) => t.name)).toEqual(['GeneratedUser'])
			expect(generatedEnumTypes.map((t) => t.name)).toEqual(['GeneratedEnum'])
		})

		it('should return empty array for kinds with no generated types', () => {
			const generatedTypes = registry.getGeneratedTypes()
			const generatedScalarTypes = generatedTypes.filter((t) => t.kind === TypeKind.SCALAR)

			expect(generatedScalarTypes).toEqual([])
		})
	})

	describe('Registry Statistics', () => {
		it('should provide accurate count of all types', () => {
			expect(registry.getAllTypes()).toHaveLength(0)

			registry.registerType('Type1', TypeKind.OBJECT, 'data1')
			expect(registry.getAllTypes()).toHaveLength(1)

			registry.registerType('Type2', TypeKind.ENUM, 'data2')
			registry.registerType('Type3', TypeKind.SCALAR, 'data3')
			expect(registry.getAllTypes()).toHaveLength(3)
		})

		it('should provide accurate count by kind', () => {
			registry.registerType('User', TypeKind.OBJECT, 'user-data')
			registry.registerType('Post', TypeKind.OBJECT, 'post-data')
			registry.registerType('UserRole', TypeKind.ENUM, 'role-data')

			expect(registry.getTypeNamesByKind(TypeKind.OBJECT)).toHaveLength(2)
			expect(registry.getTypeNamesByKind(TypeKind.ENUM)).toHaveLength(1)
			expect(registry.getTypeNamesByKind(TypeKind.SCALAR)).toHaveLength(0)
		})
	})

	describe('Edge Cases', () => {
		it('should handle null data', () => {
			registry.registerType('NullType', TypeKind.OBJECT, null as any)

			expect(registry.hasType('NullType')).toBe(true)
			expect(registry.getType('NullType')?.data).toBeNull()
		})

		it('should handle undefined data', () => {
			registry.registerType('UndefinedType', TypeKind.OBJECT, undefined as any)

			expect(registry.hasType('UndefinedType')).toBe(true)
			expect(registry.getType('UndefinedType')?.data).toBeUndefined()
		})

		it('should handle special characters in type names', () => {
			registry.registerType('Type_With-Special.Characters', TypeKind.OBJECT, 'special-data')

			expect(registry.hasType('Type_With-Special.Characters')).toBe(true)
			expect(registry.getType('Type_With-Special.Characters')?.name).toBe('Type_With-Special.Characters')
		})

		it('should handle very long type names', () => {
			const longName = 'A'.repeat(1000)
			registry.registerType(longName, TypeKind.OBJECT, 'long-name-data')

			expect(registry.hasType(longName)).toBe(true)
			expect(registry.getType(longName)?.name).toBe(longName)
		})
	})

	describe('Type Registration Overrides', () => {
		it('should allow changing type kind when re-registering', () => {
			registry.registerType('FlexibleType', TypeKind.OBJECT, 'object-data')
			expect(registry.isTypeOfKind('FlexibleType', TypeKind.OBJECT)).toBe(true)

			registry.registerType('FlexibleType', TypeKind.ENUM, 'enum-data')
			expect(registry.isTypeOfKind('FlexibleType', TypeKind.ENUM)).toBe(true)
			expect(registry.isTypeOfKind('FlexibleType', TypeKind.OBJECT)).toBe(false)
		})

		it('should allow changing generated flag when re-registering', () => {
			registry.registerType('ToggleType', TypeKind.OBJECT, 'data', false)
			expect(registry.getType('ToggleType')?.isGenerated).toBe(false)

			registry.registerType('ToggleType', TypeKind.OBJECT, 'data', true)
			expect(registry.getType('ToggleType')?.isGenerated).toBe(true)
		})
	})
})
