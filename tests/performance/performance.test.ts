import { describe, it, expect } from 'bun:test'
import { DataModel, Enum } from '@zenstackhq/sdk/ast'
import { GeneratorOrchestrator } from '@orchestrator/generator-orchestrator'
import { TestFixtures } from '../helpers'
import { OutputFormat } from '@utils/constants'

describe('Performance Tests', () => {
	const PERFORMANCE_THRESHOLD_MS = 3000

	describe('Schema Generation Performance', () => {
		it('should generate GraphQL schema with moderate complexity efficiently', async () => {
			const models: DataModel[] = []
			for (let i = 0; i < 20; i++) {
				const fields = [
					TestFixtures.createField('id', 'String'),
					TestFixtures.createField(`name${i}`, 'String'),
					TestFixtures.createField('createdAt', 'DateTime'),
					TestFixtures.createField('count', 'Int'),
					TestFixtures.createField('active', 'Boolean'),
				]

				if (i > 0) {
					fields.push(TestFixtures.createRelationField('parent', `TestModel${i - 1}`, true, false) as any)
				}

				models.push(TestFixtures.createDataModel(`TestModel${i}`, fields))
			}

			const enums: Enum[] = []
			for (let i = 0; i < 5; i++) {
				enums.push(TestFixtures.createEnum(`TestEnum${i}`, ['OPTION_A', 'OPTION_B', 'OPTION_C']))
			}

			const context = TestFixtures.createContext({
				models,
				enums,
				generateScalars: true,
				generateEnums: true,
				generateFilters: true,
				generateSorts: true,
				connectionTypes: true,
				includeRelations: true,
			})

			const startTime = Date.now()
			const orchestrator = new GeneratorOrchestrator(context, OutputFormat.GRAPHQL)
			const result = await orchestrator.generate()
			const endTime = Date.now()

			const generationTime = endTime - startTime

			expect(generationTime).toBeLessThan(PERFORMANCE_THRESHOLD_MS)
			expect(result.sdl).toBeDefined()
			expect(result.sdl?.length ?? 0).toBeGreaterThan(5000)
			expect(result.stats.objectTypes).toBe(20)
			expect(result.stats.enumTypes).toBe(5)

			console.log(`GraphQL generation completed in ${generationTime}ms`)
		})

		it('should generate TypeScript code efficiently', async () => {
			const models = [
				TestFixtures.createDataModel('User', [
					TestFixtures.createField('id', 'String'),
					TestFixtures.createField('name', 'String'),
					TestFixtures.createField('email', 'String'),
					TestFixtures.createField('age', 'Int', true),
				]),
				TestFixtures.createDataModel('Post', [
					TestFixtures.createField('id', 'String'),
					TestFixtures.createField('title', 'String'),
					TestFixtures.createField('content', 'String', true),
					TestFixtures.createRelationField('author', 'User'),
				]),
			]

			const enums = [TestFixtures.createEnum('Status', ['ACTIVE', 'INACTIVE', 'PENDING'])]

			const context = TestFixtures.createContext({
				models,
				enums,
				generateScalars: true,
				generateEnums: true,
				generateFilters: true,
				generateSorts: true,
			})

			const startTime = Date.now()
			const orchestrator = new GeneratorOrchestrator(context, OutputFormat.TYPE_GRAPHQL)
			const result = await orchestrator.generate()
			const endTime = Date.now()

			const generationTime = endTime - startTime

			expect(generationTime).toBeLessThan(PERFORMANCE_THRESHOLD_MS)
			expect(result.code).toBeDefined()
			expect(result.code?.length ?? 0).toBeGreaterThan(1000)

			console.log(`TypeScript generation completed in ${generationTime}ms`)
		})

		it('should handle multiple models with complex field types efficiently', async () => {
			const complexFields = [
				TestFixtures.createField('id', 'String'),
				TestFixtures.createField('stringField', 'String'),
				TestFixtures.createField('intField', 'Int'),
				TestFixtures.createField('floatField', 'Float'),
				TestFixtures.createField('booleanField', 'Boolean'),
				TestFixtures.createField('dateTimeField', 'DateTime'),
				TestFixtures.createField('decimalField', 'Decimal'),
				TestFixtures.createField('jsonField', 'Json'),
				TestFixtures.createField('bytesField', 'Bytes'),
				TestFixtures.createField('optionalString', 'String', true),
				TestFixtures.createField('stringArray', 'String', false, true),
				TestFixtures.createField('optionalArray', 'Int', true, true),
			]

			const models: DataModel[] = []
			for (let i = 0; i < 10; i++) {
				models.push(TestFixtures.createDataModel(`ComplexModel${i}`, [...complexFields]))
			}

			const context = TestFixtures.createContext({
				models,
				generateFilters: true,
				generateSorts: true,
				connectionTypes: false,
			})

			const startTime = Date.now()
			const orchestrator = new GeneratorOrchestrator(context, OutputFormat.GRAPHQL)
			const result = await orchestrator.generate()
			const endTime = Date.now()

			const generationTime = endTime - startTime

			expect(generationTime).toBeLessThan(PERFORMANCE_THRESHOLD_MS)
			expect(result.sdl).toContain('type ComplexModel0')
			expect(result.sdl).toContain('type ComplexModel9')
			expect(result.stats.objectTypes).toBe(10)

			console.log(`Complex field types generation completed in ${generationTime}ms`)
		})
	})

	describe('Memory Efficiency', () => {
		it('should not consume excessive memory during generation', async () => {
			const memoryBefore = process.memoryUsage()

			const models: DataModel[] = []
			for (let i = 0; i < 15; i++) {
				const fields = [TestFixtures.createField('id', 'String'), TestFixtures.createField(`field${i}`, 'String')]
				models.push(TestFixtures.createDataModel(`MemoryTestModel${i}`, fields))
			}

			const context = TestFixtures.createContext({
				models,
				generateFilters: true,
				generateSorts: false,
			})

			const orchestrator = new GeneratorOrchestrator(context, OutputFormat.GRAPHQL)
			const result = await orchestrator.generate()

			const memoryAfter = process.memoryUsage()
			const memoryIncrease = memoryAfter.heapUsed - memoryBefore.heapUsed
			const memoryIncreaseMB = memoryIncrease / (1024 * 1024)

			expect(memoryIncreaseMB).toBeLessThan(50)
			expect(result.sdl).toBeDefined()

			console.log(`Memory increase: ${memoryIncreaseMB.toFixed(2)}MB`)
		})
	})

	describe('Concurrent Operations', () => {
		it('should handle multiple generation requests concurrently', async () => {
			const context = TestFixtures.createContext({
				models: [
					TestFixtures.createDataModel('ConcurrentModel', [TestFixtures.createField('id', 'String'), TestFixtures.createField('name', 'String')]),
				],
				generateFilters: true,
				generateSorts: false,
				connectionTypes: false,
			})

			const startTime = Date.now()

			const promises = []
			for (let i = 0; i < 3; i++) {
				const orchestrator = new GeneratorOrchestrator(context, OutputFormat.GRAPHQL)
				promises.push(orchestrator.generate())
			}

			const results = await Promise.all(promises)
			const endTime = Date.now()
			const totalTime = endTime - startTime

			expect(results).toHaveLength(3)
			for (const result of results) {
				expect(result.sdl).toBeDefined()
				expect(result.stats.objectTypes).toBe(1)
			}

			expect(totalTime).toBeLessThan(PERFORMANCE_THRESHOLD_MS)

			console.log(`Concurrent generation (3 instances) completed in ${totalTime}ms`)
		})
	})

	describe('Stress Testing', () => {
		it('should handle model with many fields efficiently', async () => {
			const fields = [TestFixtures.createField('id', 'String')]

			for (let i = 0; i < 30; i++) {
				const fieldTypes = ['String', 'Int', 'Boolean', 'DateTime']
				const fieldType = fieldTypes[i % fieldTypes.length] || 'String'
				fields.push(TestFixtures.createField(`field${i}`, fieldType))
			}

			const model = TestFixtures.createDataModel('ManyFieldsModel', fields)

			const context = TestFixtures.createContext({
				models: [model],
				generateFilters: true,
				generateSorts: true,
			})

			const startTime = Date.now()
			const orchestrator = new GeneratorOrchestrator(context, OutputFormat.GRAPHQL)
			const result = await orchestrator.generate()
			const endTime = Date.now()

			const generationTime = endTime - startTime

			expect(generationTime).toBeLessThan(PERFORMANCE_THRESHOLD_MS)
			expect(result.sdl).toContain('type ManyFieldsModel')
			expect(result.sdl).toContain('field29')

			console.log(`Many fields generation completed in ${generationTime}ms`)
		})

		it('should handle enum with many values efficiently', async () => {
			const values: string[] = []
			for (let i = 0; i < 20; i++) {
				values.push(`ENUM_VALUE_${i}`)
			}

			const largeEnum = TestFixtures.createEnum('LargeEnum', values)

			const context = TestFixtures.createContext({
				models: [
					TestFixtures.createDataModel('EnumTestModel', [TestFixtures.createField('id', 'String'), TestFixtures.createField('status', 'LargeEnum')]),
				],
				enums: [largeEnum],
				generateEnums: true,
			})

			const startTime = Date.now()
			const orchestrator = new GeneratorOrchestrator(context, OutputFormat.GRAPHQL)
			const result = await orchestrator.generate()
			const endTime = Date.now()

			const generationTime = endTime - startTime

			expect(generationTime).toBeLessThan(PERFORMANCE_THRESHOLD_MS)
			expect(result.sdl).toContain('enum LargeEnum')
			expect(result.sdl).toContain('ENUM_VALUE_19')

			console.log(`Large enum generation completed in ${generationTime}ms`)
		})
	})
})
