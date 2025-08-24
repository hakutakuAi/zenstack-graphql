import { SchemaComposer } from 'graphql-compose'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { SchemaProcessor } from '@utils/schema/schema-processor'
import { UnifiedTypeMapper } from '@utils/type-mapping/unified-type-mapper'
import { GraphQLRegistry, TypeScriptRegistry } from '@utils/registry'
import { GraphQLTypeFactories } from '@utils/schema/graphql-type-factories'
import { UnifiedGeneratorContext, OutputStrategy, SortFieldDefinition, FilterFieldDefinition } from '@generators/strategies'
import { BaseGeneratorContext, GeneratorContext } from '@core/types'
import { TestFixtures } from './test-fixtures'
import { DataModel, Enum } from '@zenstackhq/sdk/ast'
import { RelationField } from '@generators/unified/unified-relation-generator'
import { TypeKind } from '@utils/registry/base-registry'

export class TestMockFactory {
	static createSchemaComposer(): SchemaComposer {
		return new SchemaComposer()
	}

	static createTypeFormatter(): TypeFormatter {
		return TypeFormatter.fromOptions('PascalCase', 'camelCase')
	}

	static createSchemaProcessor(): SchemaProcessor {
		return new SchemaProcessor()
	}

	static createUnifiedTypeMapper(context: BaseGeneratorContext = TestFixtures.createContext()): UnifiedTypeMapper {
		const typeFormatter = TestMockFactory.createTypeFormatter()
		return new UnifiedTypeMapper(typeFormatter, context.models, context.enums, context.options)
	}

	static createTypeScriptRegistry(): TypeScriptRegistry {
		return new TypeScriptRegistry()
	}

	static createGraphQLRegistry(): GraphQLRegistry {
		const schemaComposer = TestMockFactory.createSchemaComposer()
		const registry = new GraphQLRegistry(schemaComposer)

		TestMockFactory.addEssentialTypes(registry, schemaComposer)
		registry['syncFromSchemaComposer']()

		return registry
	}

	private static addEssentialTypes(registry: GraphQLRegistry, schemaComposer: SchemaComposer): void {
		registry.addRelayRequirements()

		const scalars = [
			{ name: 'DateTime', description: 'A date-time string at UTC' },
			{ name: 'JSON', description: 'The `JSON` scalar type represents JSON values' },
			{ name: 'Decimal', description: 'An arbitrary-precision Decimal type' },
		]

		scalars.forEach(({ name, description }) => {
			if (!schemaComposer.has(name)) {
				const scalarTC = schemaComposer.createScalarTC({ name, description })
				registry.registerType(name, TypeKind.SCALAR, scalarTC, true)
			}
		})

		TestMockFactory.addFilterTypes(schemaComposer)

		if (!schemaComposer.has('SortDirection')) {
			const enumTC = schemaComposer.createEnumTC({
				name: 'SortDirection',
				values: {
					ASC: { value: 'ASC' },
					DESC: { value: 'DESC' },
				},
			})
			registry.registerType('SortDirection', TypeKind.ENUM, enumTC, true)
		}
	}

	private static addFilterTypes(schemaComposer: SchemaComposer): void {
		if (!schemaComposer.has('StringFilterInput')) {
			schemaComposer.createInputTC({
				name: 'StringFilterInput',
				fields: {
					equals: 'String',
					not: 'String',
					in: '[String!]',
					notIn: '[String!]',
					contains: 'String',
					startsWith: 'String',
					endsWith: 'String',
				},
			})
		}

		if (!schemaComposer.has('IntFilterInput')) {
			schemaComposer.createInputTC({
				name: 'IntFilterInput',
				fields: {
					equals: 'Int',
					not: 'Int',
					in: '[Int!]',
					notIn: '[Int!]',
					lt: 'Int',
					lte: 'Int',
					gt: 'Int',
					gte: 'Int',
				},
			})
		}

		if (!schemaComposer.has('NumericFilterInput')) {
			schemaComposer.createInputTC({
				name: 'NumericFilterInput',
				fields: {
					equals: 'Float',
					not: 'Float',
					in: '[Float!]',
					notIn: '[Float!]',
					lt: 'Float',
					lte: 'Float',
					gt: 'Float',
					gte: 'Float',
				},
			})
		}

		if (!schemaComposer.has('BooleanFilterInput')) {
			schemaComposer.createInputTC({
				name: 'BooleanFilterInput',
				fields: {
					equals: 'Boolean',
					not: 'Boolean',
				},
			})
		}

		if (!schemaComposer.has('DateTimeFilterInput')) {
			schemaComposer.createInputTC({
				name: 'DateTimeFilterInput',
				fields: {
					equals: 'DateTime',
					not: 'DateTime',
					in: '[DateTime!]',
					notIn: '[DateTime!]',
					lt: 'DateTime',
					lte: 'DateTime',
					gt: 'DateTime',
					gte: 'DateTime',
				},
			})
		}
	}

	static createGraphQLTypeFactories(): GraphQLTypeFactories {
		const schemaComposer = TestMockFactory.createSchemaComposer()
		const typeFormatter = TestMockFactory.createTypeFormatter()
		return new GraphQLTypeFactories(schemaComposer, typeFormatter)
	}

	static createGraphQLContext(context: BaseGeneratorContext = TestFixtures.createContext()): GeneratorContext {
		const typeFormatter = TestMockFactory.createTypeFormatter()
		const registry = TestMockFactory.createGraphQLRegistry()
		const schemaComposer = registry.schemaComposer

		return {
			...context,
			schemaComposer,
			attributeProcessor: TestMockFactory.createSchemaProcessor(),
			registry,
			typeFormatter,
			typeMapper: TestMockFactory.createUnifiedTypeMapper(context),
			typeFactories: new GraphQLTypeFactories(schemaComposer, typeFormatter),
		}
	}

	static createUnifiedContext(context: BaseGeneratorContext = TestFixtures.createContext()): UnifiedGeneratorContext {
		return {
			outputStrategy: new MockOutputStrategy(),
			options: context.options,
			models: context.models,
			enums: context.enums,
			typeFormatter: TestMockFactory.createTypeFormatter(),
			attributeProcessor: TestMockFactory.createSchemaProcessor(),
			typeMapper: TestMockFactory.createUnifiedTypeMapper(context),
		}
	}

	static createSpyUnifiedContext(context: BaseGeneratorContext = TestFixtures.createContext()): UnifiedGeneratorContext & { spy: SpyOutputStrategy } {
		const spy = new SpyOutputStrategy()
		const typeFormatter = TestMockFactory.createTypeFormatter()

		return {
			outputStrategy: spy,
			options: context.options,
			models: context.models,
			enums: context.enums,
			typeFormatter,
			attributeProcessor: TestMockFactory.createSchemaProcessor(),
			typeMapper: TestMockFactory.createUnifiedTypeMapper(context),
			spy,
		}
	}
}

export class MockOutputStrategy implements OutputStrategy {
	private generatedTypes: string[] = []
	private generatedCode = ''
	private processedRelations: string[] = []

	createSortInputType(typeName: string, _fields: SortFieldDefinition[]): string {
		this.generatedTypes.push(typeName)
		return typeName
	}

	createFilterInputType(typeName: string, _fields: FilterFieldDefinition[]): string {
		this.generatedTypes.push(typeName)
		return typeName
	}

	createConnectionType(typeName: string): string {
		const connectionTypeName = `${typeName}Connection`
		this.generatedTypes.push(connectionTypeName)
		return connectionTypeName
	}

	createObjectType(typeName: string, _fields: Record<string, any>, _description?: string): string {
		this.generatedTypes.push(typeName)
		return typeName
	}

	createInputType(typeName: string, _model: DataModel, _inputType: 'create' | 'update', _description?: string): string {
		this.generatedTypes.push(typeName)
		return typeName
	}

	createQueryArgsInputType(typeName: string, _model: DataModel, _description?: string): string {
		this.generatedTypes.push(typeName)
		return typeName
	}

	createEnumType(enumType: Enum): string {
		this.generatedTypes.push(enumType.name)
		return enumType.name
	}

	createPaginationTypes(): void {
		this.generatedTypes.push('PageInfo', 'PaginationInput')
	}

	createCommonFilterTypes(): void {
		this.generatedTypes.push('StringFilterInput', 'NumericFilterInput', 'BooleanFilterInput', 'DateTimeFilterInput')
	}

	createSortDirectionEnum(): void {
		this.generatedTypes.push('SortDirection')
	}

	processRelation(relation: RelationField): void {
		const relationKey = `${relation.modelName}-${relation.targetModelName}-${relation.fieldName}`
		this.processedRelations.push(relationKey)
	}

	hasProcessedRelation(relationKey: string): boolean {
		return this.processedRelations.includes(relationKey)
	}

	getProcessedRelations(): string[] {
		return [...this.processedRelations]
	}

	hasType(name: string): boolean {
		return this.generatedTypes.includes(name)
	}

	getGeneratedTypeNames(filter?: (name: string) => boolean): string[] {
		const types = [...this.generatedTypes]
		return filter ? types.filter(filter) : types
	}

	getGeneratedCode(): string {
		return this.generatedCode
	}

	setGeneratedCode(code: string): void {
		this.generatedCode = code
	}

	reset(): void {
		this.generatedTypes = []
		this.generatedCode = ''
		this.processedRelations = []
	}
}

export class SpyOutputStrategy extends MockOutputStrategy {
	private calls: Array<{ method: string; args: any[] }> = []

	override createSortInputType(typeName: string, fields: SortFieldDefinition[]): string {
		this.calls.push({ method: 'createSortInputType', args: [typeName, fields] })
		return super.createSortInputType(typeName, fields)
	}

	override createFilterInputType(typeName: string, fields: FilterFieldDefinition[]): string {
		this.calls.push({ method: 'createFilterInputType', args: [typeName, fields] })
		return super.createFilterInputType(typeName, fields)
	}

	override createConnectionType(typeName: string): string {
		this.calls.push({ method: 'createConnectionType', args: [typeName] })
		return super.createConnectionType(typeName)
	}

	override createObjectType(typeName: string, fields: Record<string, any>, description?: string): string {
		this.calls.push({ method: 'createObjectType', args: [typeName, fields, description] })
		return super.createObjectType(typeName, fields, description)
	}

	override createInputType(typeName: string, model: DataModel, inputType: 'create' | 'update', description?: string): string {
		this.calls.push({ method: 'createInputType', args: [typeName, model, inputType, description] })
		return super.createInputType(typeName, model, inputType, description)
	}

	override createQueryArgsInputType(typeName: string, model: DataModel, description?: string): string {
		this.calls.push({ method: 'createQueryArgsInputType', args: [typeName, model, description] })
		return super.createQueryArgsInputType(typeName, model, description)
	}

	override processRelation(relation: RelationField): void {
		this.calls.push({ method: 'processRelation', args: [relation] })
		return super.processRelation(relation)
	}

	override createEnumType(enumType: Enum): string {
		this.calls.push({ method: 'createEnumType', args: [enumType] })
		return super.createEnumType(enumType)
	}

	getCalls(): Array<{ method: string; args: any[] }> {
		return [...this.calls]
	}

	getCallsForMethod(method: string): Array<{ method: string; args: any[] }> {
		return this.calls.filter((call) => call.method === method)
	}

	wasCalledWith(method: string, ...args: any[]): boolean {
		return this.calls.some((call) => call.method === method && JSON.stringify(call.args) === JSON.stringify(args))
	}

	getCallCount(method?: string): number {
		if (method) {
			return this.calls.filter((call) => call.method === method).length
		}
		return this.calls.length
	}

	override reset(): void {
		super.reset()
		this.calls = []
	}
}

export interface MockGenerationResult {
	code?: string
	sdl?: string
	items: string[]
	count: number
	type: string
}

export class MockGenerator {
	constructor(private result: MockGenerationResult = { items: [], count: 0, type: 'mock' }) {}

	generate(): string[] {
		return this.result.items
	}

	setResult(result: MockGenerationResult): void {
		this.result = result
	}
}

export class MockRegistry {
	private types = new Map<string, any>()
	private validationWarnings: string[] = []

	addType(name: string, type: any): void {
		this.types.set(name, type)
	}

	hasType(name: string): boolean {
		return this.types.has(name)
	}

	getType(name: string): any {
		return this.types.get(name)
	}

	validateSchema(): string[] {
		return [...this.validationWarnings]
	}

	addValidationWarning(warning: string): void {
		this.validationWarnings.push(warning)
	}

	clearValidationWarnings(): void {
		this.validationWarnings = []
	}

	reset(): void {
		this.types.clear()
		this.validationWarnings = []
	}
}
