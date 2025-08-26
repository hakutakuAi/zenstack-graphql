import { describe, it, expect, beforeEach } from 'bun:test'
import { SchemaComposer } from 'graphql-compose'
import { RegistryFactory, EnhancedBaseRegistry } from '@utils/registry/registry-factory'
import { GraphQLRegistry } from '@utils/registry/graphql-registry'
import { TypeScriptRegistry } from '@utils/registry/typescript-registry'
import { TypeKind, BaseTypeInfo } from '@utils/registry/base-registry'

describe('RegistryFactory', () => {
	let schemaComposer: SchemaComposer<unknown>

	beforeEach(() => {
		schemaComposer = new SchemaComposer()
	})

	describe('createRegistry', () => {
		it('should create GraphQL registry with schema composer', () => {
			const registry = RegistryFactory.createRegistry({
				format: 'graphql',
				schemaComposer,
			})

			expect(registry).toBeInstanceOf(GraphQLRegistry)
		})

		it('should throw error for GraphQL registry without schema composer', () => {
			expect(() => {
				RegistryFactory.createRegistry({
					format: 'graphql',
				})
			}).toThrow('SchemaComposer is required for GraphQL registry')
		})

		it('should create TypeScript registry', () => {
			const registry = RegistryFactory.createRegistry({
				format: 'type-graphql',
			})

			expect(registry).toBeInstanceOf(TypeScriptRegistry)
		})

		it('should throw error for unsupported format', () => {
			expect(() => {
				RegistryFactory.createRegistry({
					format: 'unsupported' as any,
				})
			}).toThrow('Unsupported registry format: unsupported')
		})
	})

	describe('createGraphQLRegistry', () => {
		it('should create GraphQL registry directly', () => {
			const registry = RegistryFactory.createGraphQLRegistry(schemaComposer)

			expect(registry).toBeInstanceOf(GraphQLRegistry)
		})
	})

	describe('createTypeScriptRegistry', () => {
		it('should create TypeScript registry directly', () => {
			const registry = RegistryFactory.createTypeScriptRegistry()

			expect(registry).toBeInstanceOf(TypeScriptRegistry)
		})
	})

	describe('getSupportedFormats', () => {
		it('should return supported formats', () => {
			const formats = RegistryFactory.getSupportedFormats()

			expect(formats).toEqual(['graphql', 'type-graphql'])
			expect(formats.length).toBe(2)
		})
	})

	describe('isFormatSupported', () => {
		it('should return true for supported formats', () => {
			expect(RegistryFactory.isFormatSupported('graphql')).toBe(true)
			expect(RegistryFactory.isFormatSupported('type-graphql')).toBe(true)
		})

		it('should return false for unsupported formats', () => {
			expect(RegistryFactory.isFormatSupported('unsupported')).toBe(false)
			expect(RegistryFactory.isFormatSupported('invalid')).toBe(false)
			expect(RegistryFactory.isFormatSupported('')).toBe(false)
		})
	})
})

describe('EnhancedBaseRegistry', () => {
	class TestEnhancedRegistry extends EnhancedBaseRegistry<string, BaseTypeInfo<string>> {
		protected createTypeInfo(name: string, kind: TypeKind, data: string, isGenerated: boolean): BaseTypeInfo<string> {
			return {
				name,
				kind,
				data,
				isGenerated,
			}
		}

		validateSchema(): string[] {
			return []
		}
	}

	let registry: TestEnhancedRegistry

	beforeEach(() => {
		registry = new TestEnhancedRegistry()
	})

	describe('getTypesByKindGrouped', () => {
		it('should group types by kind', () => {
			registry.registerType('User', TypeKind.OBJECT, 'user-data')
			registry.registerType('Post', TypeKind.OBJECT, 'post-data')
			registry.registerType('Status', TypeKind.ENUM, 'enum-data')
			registry.registerType('ID', TypeKind.SCALAR, 'scalar-data')

			const grouped = registry.getTypesByKindGrouped()

			expect(grouped[TypeKind.OBJECT]).toHaveLength(2)
			expect(grouped[TypeKind.ENUM]).toHaveLength(1)
			expect(grouped[TypeKind.SCALAR]).toHaveLength(1)
			expect(grouped[TypeKind.OBJECT]?.[0]?.name).toBe('User')
			expect(grouped[TypeKind.OBJECT]?.[1]?.name).toBe('Post')
		})

		it('should return empty groups for registry with no types', () => {
			const grouped = registry.getTypesByKindGrouped()

			expect(Object.keys(grouped)).toHaveLength(0)
		})

		it('should handle single type correctly', () => {
			registry.registerType('SingleType', TypeKind.OBJECT, 'single-data')

			const grouped = registry.getTypesByKindGrouped()

			expect(grouped[TypeKind.OBJECT]).toHaveLength(1)
			expect(grouped[TypeKind.OBJECT]?.[0]?.name).toBe('SingleType')
		})
	})

	describe('getTypeStatistics', () => {
		it('should return statistics for all type kinds', () => {
			registry.registerType('User', TypeKind.OBJECT, 'user-data')
			registry.registerType('Post', TypeKind.OBJECT, 'post-data')
			registry.registerType('Status', TypeKind.ENUM, 'enum-data')
			registry.registerType('ID', TypeKind.SCALAR, 'scalar-data')

			const stats = registry.getTypeStatistics()

			expect(stats[TypeKind.OBJECT]).toBe(2)
			expect(stats[TypeKind.ENUM]).toBe(1)
			expect(stats[TypeKind.SCALAR]).toBe(1)
			expect(stats[TypeKind.INTERFACE]).toBe(0)
			expect(stats[TypeKind.UNION]).toBe(0)
		})

		it('should return zero counts for empty registry', () => {
			const stats = registry.getTypeStatistics()

			expect(stats[TypeKind.OBJECT]).toBe(0)
			expect(stats[TypeKind.ENUM]).toBe(0)
			expect(stats[TypeKind.SCALAR]).toBe(0)
			expect(stats[TypeKind.INPUT]).toBe(0)
		})

		it('should include all TypeKind values in statistics', () => {
			const stats = registry.getTypeStatistics()
			const expectedKinds = Object.values(TypeKind)

			expect(Object.keys(stats)).toHaveLength(expectedKinds.length)

			for (const kind of expectedKinds) {
				expect(stats[kind]).toBeDefined()
				expect(typeof stats[kind]).toBe('number')
			}
		})
	})

	describe('hasAnyTypes', () => {
		it('should return false for empty registry', () => {
			expect(registry.hasAnyTypes()).toBe(false)
		})

		it('should return true when registry has types', () => {
			registry.registerType('User', TypeKind.OBJECT, 'user-data')

			expect(registry.hasAnyTypes()).toBe(true)
		})

		it('should return false after clearing all types', () => {
			registry.registerType('User', TypeKind.OBJECT, 'user-data')
			registry.clearAllTypes()

			expect(registry.hasAnyTypes()).toBe(false)
		})
	})

	describe('getTotalTypeCount', () => {
		it('should return zero for empty registry', () => {
			expect(registry.getTotalTypeCount()).toBe(0)
		})

		it('should return correct count with multiple types', () => {
			registry.registerType('User', TypeKind.OBJECT, 'user-data')
			registry.registerType('Post', TypeKind.OBJECT, 'post-data')
			registry.registerType('Status', TypeKind.ENUM, 'enum-data')

			expect(registry.getTotalTypeCount()).toBe(3)
		})

		it('should update count when types are added', () => {
			expect(registry.getTotalTypeCount()).toBe(0)

			registry.registerType('Type1', TypeKind.OBJECT, 'data1')
			expect(registry.getTotalTypeCount()).toBe(1)

			registry.registerType('Type2', TypeKind.ENUM, 'data2')
			expect(registry.getTotalTypeCount()).toBe(2)
		})
	})

	describe('clearAllTypes', () => {
		it('should clear all types and processed items', () => {
			registry.registerType('User', TypeKind.OBJECT, 'user-data')
			registry.registerType('Post', TypeKind.OBJECT, 'post-data')

			expect(registry.getTotalTypeCount()).toBe(2)
			expect(registry.hasAnyTypes()).toBe(true)

			registry.clearAllTypes()

			expect(registry.getTotalTypeCount()).toBe(0)
			expect(registry.hasAnyTypes()).toBe(false)
		})

		it('should allow adding types after clearing', () => {
			registry.registerType('User', TypeKind.OBJECT, 'user-data')
			registry.clearAllTypes()

			registry.registerType('NewType', TypeKind.SCALAR, 'new-data')

			expect(registry.getTotalTypeCount()).toBe(1)
			expect(registry.hasType('NewType')).toBe(true)
		})
	})

	describe('bulkRegisterTypes', () => {
		it('should register multiple types at once', () => {
			const registrations = [
				{
					name: 'User',
					kind: TypeKind.OBJECT,
					data: 'user-data',
				},
				{
					name: 'Post',
					kind: TypeKind.OBJECT,
					data: 'post-data',
					isGenerated: false,
				},
				{
					name: 'Status',
					kind: TypeKind.ENUM,
					data: 'enum-data',
				},
			]

			registry.bulkRegisterTypes(registrations)

			expect(registry.getTotalTypeCount()).toBe(3)
			expect(registry.hasType('User')).toBe(true)
			expect(registry.hasType('Post')).toBe(true)
			expect(registry.hasType('Status')).toBe(true)
		})

		it('should handle empty registrations array', () => {
			registry.bulkRegisterTypes([])

			expect(registry.getTotalTypeCount()).toBe(0)
		})

		it('should respect isGenerated flag', () => {
			const registrations = [
				{
					name: 'Generated',
					kind: TypeKind.OBJECT,
					data: 'generated-data',
					isGenerated: true,
				},
				{
					name: 'Manual',
					kind: TypeKind.OBJECT,
					data: 'manual-data',
					isGenerated: false,
				},
			]

			registry.bulkRegisterTypes(registrations)

			const generated = registry.getGeneratedTypes()
			const generatedNames = generated.map((t) => t.name)

			expect(generatedNames).toContain('Generated')
			expect(generatedNames).not.toContain('Manual')
		})

		it('should default isGenerated to true when not specified', () => {
			const registrations = [
				{
					name: 'DefaultGenerated',
					kind: TypeKind.OBJECT,
					data: 'data',
				},
			]

			registry.bulkRegisterTypes(registrations)

			const generated = registry.getGeneratedTypes()
			const generatedNames = generated.map((t) => t.name)

			expect(generatedNames).toContain('DefaultGenerated')
		})
	})

	describe('findTypesByPattern', () => {
		beforeEach(() => {
			registry.registerType('UserInput', TypeKind.INPUT, 'user-input')
			registry.registerType('PostInput', TypeKind.INPUT, 'post-input')
			registry.registerType('UserFilterInput', TypeKind.INPUT, 'user-filter')
			registry.registerType('Post', TypeKind.OBJECT, 'post-object')
			registry.registerType('UserConnection', TypeKind.CONNECTION, 'user-connection')
		})

		it('should find types matching pattern', () => {
			const pattern = /Input$/
			const results = registry.findTypesByPattern(pattern)

			expect(results).toHaveLength(3)
			const names = results.map((t) => t.name)
			expect(names).toContain('UserInput')
			expect(names).toContain('PostInput')
			expect(names).toContain('UserFilterInput')
		})

		it('should find types with prefix pattern', () => {
			const pattern = /^User/
			const results = registry.findTypesByPattern(pattern)

			expect(results).toHaveLength(3)
			const names = results.map((t) => t.name)
			expect(names).toContain('UserInput')
			expect(names).toContain('UserFilterInput')
			expect(names).toContain('UserConnection')
		})

		it('should return empty array for no matches', () => {
			const pattern = /NonExistent/
			const results = registry.findTypesByPattern(pattern)

			expect(results).toHaveLength(0)
		})

		it('should handle complex regex patterns', () => {
			const pattern = /^(User|Post).*Input$/
			const results = registry.findTypesByPattern(pattern)

			expect(results).toHaveLength(3)
			const names = results.map((t) => t.name)
			expect(names).toContain('UserInput')
			expect(names).toContain('PostInput')
			expect(names).toContain('UserFilterInput')
		})
	})

	describe('getDependencyChain', () => {
		beforeEach(() => {
			const userType: BaseTypeInfo<string> = {
				name: 'User',
				kind: TypeKind.OBJECT,
				data: 'user-data',
				isGenerated: true,
				dependencies: ['Profile', 'Post'],
			}

			const profileType: BaseTypeInfo<string> = {
				name: 'Profile',
				kind: TypeKind.OBJECT,
				data: 'profile-data',
				isGenerated: true,
				dependencies: ['Address'],
			}

			const addressType: BaseTypeInfo<string> = {
				name: 'Address',
				kind: TypeKind.OBJECT,
				data: 'address-data',
				isGenerated: true,
				dependencies: [],
			}

			const postType: BaseTypeInfo<string> = {
				name: 'Post',
				kind: TypeKind.OBJECT,
				data: 'post-data',
				isGenerated: true,
				dependencies: [],
			}

			registry['types'].set('User', userType)
			registry['types'].set('Profile', profileType)
			registry['types'].set('Address', addressType)
			registry['types'].set('Post', postType)
		})

		it('should return complete dependency chain', () => {
			const chain = registry.getDependencyChain('User')

			expect(chain).toContain('Address')
			expect(chain).toContain('Profile')
			expect(chain).toContain('Post')
		})

		it('should return empty array for type with no dependencies', () => {
			const chain = registry.getDependencyChain('Post')

			expect(chain).toEqual([])
		})

		it('should return empty array for non-existent type', () => {
			const chain = registry.getDependencyChain('NonExistent')

			expect(chain).toEqual([])
		})

		it('should handle circular dependencies gracefully', () => {
			const typeA: BaseTypeInfo<string> = {
				name: 'TypeA',
				kind: TypeKind.OBJECT,
				data: 'data-a',
				isGenerated: true,
				dependencies: ['TypeB'],
			}

			const typeB: BaseTypeInfo<string> = {
				name: 'TypeB',
				kind: TypeKind.OBJECT,
				data: 'data-b',
				isGenerated: true,
				dependencies: ['TypeC'],
			}

			const typeC: BaseTypeInfo<string> = {
				name: 'TypeC',
				kind: TypeKind.OBJECT,
				data: 'data-c',
				isGenerated: true,
				dependencies: ['TypeA'],
			}

			registry['types'].set('TypeA', typeA)
			registry['types'].set('TypeB', typeB)
			registry['types'].set('TypeC', typeC)

			const chain = registry.getDependencyChain('TypeA')

			expect(chain.length).toBeGreaterThanOrEqual(2)
			expect(chain).toContain('TypeB')
			expect(chain).toContain('TypeC')
		})

		it('should not include duplicates in dependency chain', () => {
			const typeD: BaseTypeInfo<string> = {
				name: 'TypeD',
				kind: TypeKind.OBJECT,
				data: 'data-d',
				isGenerated: true,
				dependencies: [],
			}

			const typeB: BaseTypeInfo<string> = {
				name: 'TypeB',
				kind: TypeKind.OBJECT,
				data: 'data-b',
				isGenerated: true,
				dependencies: ['TypeD'],
			}

			const typeC: BaseTypeInfo<string> = {
				name: 'TypeC',
				kind: TypeKind.OBJECT,
				data: 'data-c',
				isGenerated: true,
				dependencies: ['TypeD'],
			}

			const typeA: BaseTypeInfo<string> = {
				name: 'TypeA',
				kind: TypeKind.OBJECT,
				data: 'data-a',
				isGenerated: true,
				dependencies: ['TypeB', 'TypeC'],
			}

			registry['types'].set('TypeA', typeA)
			registry['types'].set('TypeB', typeB)
			registry['types'].set('TypeC', typeC)
			registry['types'].set('TypeD', typeD)

			const chain = registry.getDependencyChain('TypeA')

			const typeDCount = chain.filter((dep) => dep === 'TypeD').length
			expect(typeDCount).toBe(1)
			expect(chain).toContain('TypeB')
			expect(chain).toContain('TypeC')
			expect(chain).toContain('TypeD')
		})
	})
})
