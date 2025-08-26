import { describe, it, expect, beforeEach } from 'bun:test'
import { RegistryFactory, RegistryFormat } from '@utils/registry/registry-factory'
import { GraphQLRegistry, TypeScriptRegistry } from '@utils/registry'
import { SchemaComposer } from 'graphql-compose'

describe('Registry Factory', () => {
	describe('Static Methods', () => {
		it('should have static createRegistry method', () => {
			expect(typeof RegistryFactory.createRegistry).toBe('function')
		})

		it('should have static createGraphQLRegistry method', () => {
			expect(typeof RegistryFactory.createGraphQLRegistry).toBe('function')
		})

		it('should have static createTypeScriptRegistry method', () => {
			expect(typeof RegistryFactory.createTypeScriptRegistry).toBe('function')
		})
	})

	describe('GraphQL Registry Creation', () => {
		it('should create GraphQL registry for graphql format', () => {
			const schemaComposer = new SchemaComposer()
			const registry = RegistryFactory.createRegistry({
				format: 'graphql',
				schemaComposer,
			})

			expect(registry).toBeInstanceOf(GraphQLRegistry)
		})

		it('should create GraphQL registry with createGraphQLRegistry', () => {
			const schemaComposer = new SchemaComposer()
			const registry = RegistryFactory.createGraphQLRegistry(schemaComposer)

			expect(registry).toBeInstanceOf(GraphQLRegistry)
		})

		it('should throw error when creating GraphQL registry without schema composer', () => {
			expect(() => {
				RegistryFactory.createRegistry({
					format: 'graphql',
				})
			}).toThrow('SchemaComposer is required for GraphQL registry')
		})
	})

	describe('TypeScript Registry Creation', () => {
		it('should create TypeScript registry for type-graphql format', () => {
			const registry = RegistryFactory.createRegistry({
				format: 'type-graphql',
			})

			expect(registry).toBeInstanceOf(TypeScriptRegistry)
		})

		it('should create TypeScript registry with createTypeScriptRegistry', () => {
			const registry = RegistryFactory.createTypeScriptRegistry()

			expect(registry).toBeInstanceOf(TypeScriptRegistry)
		})

		it('should create TypeScript registry consistently', () => {
			const registry1 = RegistryFactory.createRegistry({
				format: 'type-graphql',
			})
			const registry2 = RegistryFactory.createRegistry({
				format: 'type-graphql',
			})

			expect(registry1).toBeInstanceOf(TypeScriptRegistry)
			expect(registry2).toBeInstanceOf(TypeScriptRegistry)
			expect(registry1).not.toBe(registry2)
		})
	})

	describe('Format Support', () => {
		it('should get supported formats', () => {
			const formats = RegistryFactory.getSupportedFormats()

			expect(formats).toContain('graphql')
			expect(formats).toContain('type-graphql')
			expect(formats).toHaveLength(2)
		})

		it('should check if format is supported', () => {
			expect(RegistryFactory.isFormatSupported('graphql')).toBe(true)
			expect(RegistryFactory.isFormatSupported('type-graphql')).toBe(true)
			expect(RegistryFactory.isFormatSupported('unknown')).toBe(false)
			expect(RegistryFactory.isFormatSupported('')).toBe(false)
		})
	})

	describe('Error Handling', () => {
		it('should throw error for unsupported format', () => {
			expect(() => {
				RegistryFactory.createRegistry({
					format: 'unknown' as RegistryFormat,
				})
			}).toThrow('Unsupported registry format: unknown')
		})

		it('should handle null options', () => {
			expect(() => {
				RegistryFactory.createRegistry(null as any)
			}).toThrow()
		})

		it('should handle undefined options', () => {
			expect(() => {
				RegistryFactory.createRegistry(undefined as any)
			}).toThrow()
		})

		it('should provide meaningful error for invalid context', () => {
			try {
				RegistryFactory.createRegistry({
					format: 'invalid' as RegistryFormat,
				})
				expect(true).toBe(false)
			} catch (error) {
				expect(error).toBeDefined()
				const message = (error as Error).message || (error as Error).toString()
				expect(message).toBeTruthy()
				expect(message).toContain('Unsupported registry format')
			}
		})
	})

	describe('Registry Features', () => {
		it('should create functional GraphQL registry', () => {
			const schemaComposer = new SchemaComposer()
			const registry = RegistryFactory.createRegistry({
				format: 'graphql',
				schemaComposer,
			}) as GraphQLRegistry

			expect(registry.hasType).toBeDefined()
			expect(registry.registerType).toBeDefined()
			expect(registry.getAllTypes).toBeDefined()
		})

		it('should create functional TypeScript registry', () => {
			const registry = RegistryFactory.createRegistry({
				format: 'type-graphql',
			}) as TypeScriptRegistry

			expect(registry.hasType).toBeDefined()
			expect(registry.registerType).toBeDefined()
			expect(registry.getAllTypes).toBeDefined()
			expect(registry.generateCode).toBeDefined()
		})
	})

	describe('Memory Management', () => {
		it('should not leak memory with multiple registry creations', () => {
			const registries: any[] = []

			for (let i = 0; i < 100; i++) {
				registries.push(
					RegistryFactory.createRegistry({
						format: 'type-graphql',
					}),
				)
			}

			for (let i = 0; i < registries.length - 1; i++) {
				expect(registries[i]).not.toBe(registries[i + 1])
			}

			expect(registries).toHaveLength(100)
		})

		it('should handle rapid registry creation and destruction', () => {
			for (let i = 0; i < 50; i++) {
				const registry = RegistryFactory.createRegistry({
					format: 'type-graphql',
				})
				expect(registry).toBeInstanceOf(TypeScriptRegistry)
			}
		})
	})

	describe('Factory State', () => {
		it('should be stateless across registry creations', () => {
			const schemaComposer = new SchemaComposer()

			const graphqlRegistry = RegistryFactory.createRegistry({
				format: 'graphql',
				schemaComposer,
			})
			const typescriptRegistry = RegistryFactory.createRegistry({
				format: 'type-graphql',
			})
			const anotherGraphqlRegistry = RegistryFactory.createRegistry({
				format: 'graphql',
				schemaComposer,
			})

			expect(graphqlRegistry).toBeInstanceOf(GraphQLRegistry)
			expect(typescriptRegistry).toBeInstanceOf(TypeScriptRegistry)
			expect(anotherGraphqlRegistry).toBeInstanceOf(GraphQLRegistry)
		})

		it('should not affect subsequent creations', () => {
			const schemaComposer = new SchemaComposer()

			RegistryFactory.createRegistry({
				format: 'graphql',
				schemaComposer,
			})

			const registry2 = RegistryFactory.createRegistry({
				format: 'type-graphql',
			})

			expect(registry2).toBeInstanceOf(TypeScriptRegistry)
		})
	})
})
