import { describe, it, expect, beforeEach } from 'bun:test'
import { UnifiedGeneratorFactory } from '@generators/unified/unified-generator-factory'
import { TestFixtures } from '../../helpers'
import { BaseGeneratorContext, GeneratorContext } from '@core/types'
import { SchemaComposer } from 'graphql-compose'
import { GraphQLRegistry } from '@utils/registry'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { SchemaProcessor } from '@utils/schema/schema-processor'
import { UnifiedTypeMapper } from '@utils/type-mapping/unified-type-mapper'
import { GraphQLTypeFactories } from '@utils/schema/graphql-type-factories'

describe('UnifiedGeneratorFactory', () => {
	let baseContext: BaseGeneratorContext
	let graphqlContext: GeneratorContext

	beforeEach(() => {
		baseContext = TestFixtures.createContext({
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
			enums: [TestFixtures.createEnum('UserRole', ['ADMIN', 'USER', 'MODERATOR']), TestFixtures.createEnum('Status', ['ACTIVE', 'INACTIVE'])],
		})

		const schemaComposer = new SchemaComposer()
		const registry = new GraphQLRegistry(schemaComposer)
		const typeFormatter = new TypeFormatter(baseContext.options.typeNaming, baseContext.options.fieldNaming)
		const attributeProcessor = new SchemaProcessor()
		const typeMapper = new UnifiedTypeMapper(typeFormatter, baseContext.models, baseContext.enums, baseContext.options)
		const typeFactories = new GraphQLTypeFactories(schemaComposer, typeFormatter)

		graphqlContext = {
			...baseContext,
			schemaComposer,
			registry,
			attributeProcessor,
			typeFormatter,
			typeMapper,
			typeFactories,
		}
	})

	describe('GraphQL Generators Creation', () => {
		it('should create all GraphQL generators successfully', () => {
			const generators = UnifiedGeneratorFactory.createGraphQLGenerators(graphqlContext)

			expect(generators).toBeDefined()
			expect(generators.sortInputGenerator).toBeDefined()
			expect(generators.filterInputGenerator).toBeDefined()
			expect(generators.connectionGenerator).toBeDefined()
			expect(generators.objectTypeGenerator).toBeDefined()
			expect(generators.queryArgsGenerator).toBeDefined()
			expect(generators.enumGenerator).toBeDefined()
			expect(generators.scalarGenerator).toBeDefined()
			expect(generators.relationGenerator).toBeDefined()

			expect(generators.sortInputGenerator.constructor.name).toBe('UnifiedSortInputGenerator')
			expect(generators.filterInputGenerator.constructor.name).toBe('UnifiedFilterInputGenerator')
			expect(generators.connectionGenerator.constructor.name).toBe('UnifiedConnectionGenerator')
			expect(generators.objectTypeGenerator.constructor.name).toBe('UnifiedObjectTypeGenerator')
			expect(generators.queryArgsGenerator.constructor.name).toBe('UnifiedQueryArgsGenerator')
			expect(generators.enumGenerator.constructor.name).toBe('UnifiedEnumGenerator')
			expect(generators.scalarGenerator.constructor.name).toBe('UnifiedScalarGenerator')
			expect(generators.relationGenerator.constructor.name).toBe('UnifiedRelationGenerator')
		})

		it('should create generators with correct GraphQL output format', () => {
			const generators = UnifiedGeneratorFactory.createGraphQLGenerators(graphqlContext)

			expect(generators.enumGenerator).toBeDefined()

			const enumResult = generators.enumGenerator.generate()
			expect(enumResult).toBeDefined()
			expect(Array.isArray(enumResult)).toBe(true)
		})

		it('should create generators that can process models', () => {
			const generators = UnifiedGeneratorFactory.createGraphQLGenerators(graphqlContext)

			const objectTypeResult = generators.objectTypeGenerator.generate()
			expect(objectTypeResult).toBeDefined()
			expect(objectTypeResult.length).toBeGreaterThan(0)
			expect(Array.isArray(objectTypeResult)).toBe(true)
		})

		it('should create generators that respect context configuration', () => {
			const contextWithoutFilters = {
				...graphqlContext,
				options: {
					...graphqlContext.options,
					generateFilters: false,
					generateSorts: true,
				},
			}

			const generators = UnifiedGeneratorFactory.createGraphQLGenerators(contextWithoutFilters)

			const filterResult = generators.filterInputGenerator.generate()
			const sortResult = generators.sortInputGenerator.generate()

			expect(filterResult).toBeDefined()
			expect(sortResult).toBeDefined()
			expect(Array.isArray(sortResult)).toBe(true)
		})
	})

	describe('TypeScript Generators Creation', () => {
		it('should create all TypeScript generators successfully', () => {
			const generators = UnifiedGeneratorFactory.createTypeScriptGenerators(baseContext)

			expect(generators).toBeDefined()
			expect(generators.sortInputGenerator).toBeDefined()
			expect(generators.filterInputGenerator).toBeDefined()
			expect(generators.connectionGenerator).toBeDefined()
			expect(generators.objectTypeGenerator).toBeDefined()
			expect(generators.queryArgsGenerator).toBeDefined()
			expect(generators.enumGenerator).toBeDefined()
			expect(generators.scalarGenerator).toBeDefined()
			expect(generators.relationGenerator).toBeDefined()
			expect(generators.inputGenerator).toBeDefined()

			expect(generators.inputGenerator).toBeDefined()
			expect(generators.inputGenerator.constructor.name).toBe('UnifiedInputGenerator')
		})

		it('should create generators with correct TypeScript output format', () => {
			const generators = UnifiedGeneratorFactory.createTypeScriptGenerators(baseContext)

			expect(generators.enumGenerator).toBeDefined()

			const enumResult = generators.enumGenerator.generate()
			expect(enumResult).toBeDefined()
			expect(Array.isArray(enumResult)).toBe(true)
		})

		it('should create input generator only for TypeScript format', () => {
			const graphqlGenerators = UnifiedGeneratorFactory.createGraphQLGenerators(graphqlContext)
			const typescriptGenerators = UnifiedGeneratorFactory.createTypeScriptGenerators(baseContext)

			expect('inputGenerator' in graphqlGenerators).toBe(false)
			expect(typescriptGenerators.inputGenerator).toBeDefined()
		})

		it('should create generators that can process TypeScript context', () => {
			const generators = UnifiedGeneratorFactory.createTypeScriptGenerators(baseContext)

			const objectTypeResult = generators.objectTypeGenerator.generate()
			const inputResult = generators.inputGenerator.generate()

			expect(objectTypeResult).toBeDefined()
			expect(objectTypeResult.length).toBeGreaterThan(0)
			expect(inputResult).toBeDefined()
			expect(inputResult.length).toBeGreaterThan(0)
		})
	})

	describe('Individual Generator Creation', () => {
		it('should create individual GraphQL generator by type', () => {
			const sortGenerator = UnifiedGeneratorFactory.createGraphQLGenerator(graphqlContext, 'sortInputGenerator')
			const filterGenerator = UnifiedGeneratorFactory.createGraphQLGenerator(graphqlContext, 'filterInputGenerator')
			const objectGenerator = UnifiedGeneratorFactory.createGraphQLGenerator(graphqlContext, 'objectTypeGenerator')

			expect(sortGenerator).toBeDefined()
			expect(filterGenerator).toBeDefined()
			expect(objectGenerator).toBeDefined()

			expect(sortGenerator.constructor.name).toBe('UnifiedSortInputGenerator')
			expect(filterGenerator.constructor.name).toBe('UnifiedFilterInputGenerator')
			expect(objectGenerator.constructor.name).toBe('UnifiedObjectTypeGenerator')
		})

		it('should create individual TypeScript generator by type', () => {
			const sortGenerator = UnifiedGeneratorFactory.createTypeScriptGenerator(baseContext, 'sortInputGenerator')
			const inputGenerator = UnifiedGeneratorFactory.createTypeScriptGenerator(baseContext, 'inputGenerator')
			const enumGenerator = UnifiedGeneratorFactory.createTypeScriptGenerator(baseContext, 'enumGenerator')

			expect(sortGenerator).toBeDefined()
			expect(inputGenerator).toBeDefined()
			expect(enumGenerator).toBeDefined()

			expect(sortGenerator.constructor.name).toBe('UnifiedSortInputGenerator')
			expect(inputGenerator.constructor.name).toBe('UnifiedInputGenerator')
			expect(enumGenerator.constructor.name).toBe('UnifiedEnumGenerator')
		})

		it('should return the same generator instance as bulk creation', () => {
			const allGenerators = UnifiedGeneratorFactory.createGraphQLGenerators(graphqlContext)
			const individualGenerator = UnifiedGeneratorFactory.createGraphQLGenerator(graphqlContext, 'enumGenerator')

			expect(individualGenerator.constructor.name).toBe(allGenerators.enumGenerator.constructor.name)
		})
	})

	describe('Error Handling and Edge Cases', () => {
		it('should handle context with no models gracefully', () => {
			const emptyContext: BaseGeneratorContext = TestFixtures.createContext({
				models: [],
				enums: [],
			})

			const generators = UnifiedGeneratorFactory.createTypeScriptGenerators(emptyContext)
			expect(generators).toBeDefined()
			expect(generators.objectTypeGenerator).toBeDefined()

			const result = generators.objectTypeGenerator.generate()
			expect(result).toBeDefined()
			expect(result.length).toBe(0)
		})

		it('should handle context with no enums gracefully', () => {
			const contextWithoutEnums: BaseGeneratorContext = TestFixtures.createContext({
				models: [TestFixtures.createDataModel('User', [TestFixtures.createField('id', 'String')])],
				enums: [],
			})

			const generators = UnifiedGeneratorFactory.createGraphQLGenerators({
				...graphqlContext,
				...contextWithoutEnums,
			})

			expect(generators).toBeDefined()
			expect(generators.enumGenerator).toBeDefined()

			const result = generators.enumGenerator.generate()
			expect(result).toBeDefined()
			expect(result.length).toBe(0)
		})

		it('should create generators with different configurations', () => {
			const configA: BaseGeneratorContext = TestFixtures.createContext({
				generateScalars: true,
				generateEnums: true,
				generateFilters: true,
				generateSorts: true,
				connectionTypes: true,
				models: [TestFixtures.createDataModel('ModelA', [TestFixtures.createField('id', 'String')])],
			})

			const configB: BaseGeneratorContext = TestFixtures.createContext({
				generateScalars: false,
				generateEnums: false,
				generateFilters: false,
				generateSorts: false,
				connectionTypes: false,
				models: [TestFixtures.createDataModel('ModelB', [TestFixtures.createField('id', 'String')])],
			})

			const generatorsA = UnifiedGeneratorFactory.createTypeScriptGenerators(configA)
			const generatorsB = UnifiedGeneratorFactory.createTypeScriptGenerators(configB)

			expect(generatorsA).toBeDefined()
			expect(generatorsB).toBeDefined()

			const resultA = generatorsA.objectTypeGenerator.generate()
			const resultB = generatorsB.objectTypeGenerator.generate()

			expect(resultA.length).toBe(1)
			expect(resultB.length).toBe(1)
		})
	})

	describe('Generator Functionality Validation', () => {
		it('should create generators that produce consistent results', () => {
			const createFreshGraphQLContext = () => {
				const freshBaseContext = TestFixtures.createContext({
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
				})

				const schemaComposer = new SchemaComposer()
				const registry = new GraphQLRegistry(schemaComposer)
				const typeFormatter = new TypeFormatter(freshBaseContext.options.typeNaming, freshBaseContext.options.fieldNaming)
				const attributeProcessor = new SchemaProcessor()
				const typeMapper = new UnifiedTypeMapper(typeFormatter, freshBaseContext.models, freshBaseContext.enums, freshBaseContext.options)
				const typeFactories = new GraphQLTypeFactories(schemaComposer, typeFormatter)

				return {
					...freshBaseContext,
					schemaComposer,
					registry,
					attributeProcessor,
					typeFormatter,
					typeMapper,
					typeFactories,
				}
			}

			const generators1 = UnifiedGeneratorFactory.createGraphQLGenerators(createFreshGraphQLContext())
			const generators2 = UnifiedGeneratorFactory.createGraphQLGenerators(createFreshGraphQLContext())

			const result1 = generators1.objectTypeGenerator.generate()
			const result2 = generators2.objectTypeGenerator.generate()

			expect(result1.length).toBe(result2.length)
			expect(Array.isArray(result1)).toBe(true)
			expect(Array.isArray(result2)).toBe(true)
		})

		it('should create generators that work with complex models', () => {
			const complexContext: BaseGeneratorContext = TestFixtures.createContext({
				models: [
					TestFixtures.createDataModel('User', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('email', 'String'),
						TestFixtures.createField('name', 'String'),
						TestFixtures.createField('age', 'Int', true),
						TestFixtures.createField('active', 'Boolean'),
						TestFixtures.createField('createdAt', 'DateTime'),
						TestFixtures.createField('metadata', 'Json', true),
					]),
					TestFixtures.createDataModel('Profile', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('bio', 'String', true),
						TestFixtures.createField('avatar', 'String', true),
						TestFixtures.createRelationField('user', 'User'),
					]),
				],
				enums: [
					TestFixtures.createEnum('UserRole', ['ADMIN', 'USER', 'MODERATOR', 'GUEST']),
					TestFixtures.createEnum('Priority', ['HIGH', 'MEDIUM', 'LOW']),
					TestFixtures.createEnum('Status', ['ACTIVE', 'INACTIVE', 'PENDING', 'SUSPENDED']),
				],
			})

			const generators = UnifiedGeneratorFactory.createTypeScriptGenerators(complexContext)

			const objectResult = generators.objectTypeGenerator.generate()
			const enumResult = generators.enumGenerator.generate()
			const inputResult = generators.inputGenerator.generate()

			expect(objectResult.length).toBe(2)
			expect(enumResult.length).toBe(3)
			expect(inputResult.length).toBeGreaterThan(0)
		})
	})
})
