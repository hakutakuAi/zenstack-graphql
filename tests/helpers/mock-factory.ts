import { SchemaComposer } from 'graphql-compose'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { SchemaProcessor } from '@utils/schema/schema-processor'
import { UnifiedTypeMapper } from '@utils/type-mapping/unified-type-mapper'
import { GraphQLRegistry } from '@utils/registry'
import { TypeScriptRegistry } from '@utils/registry'
import { GraphQLTypeFactories } from '@utils/schema/graphql-type-factories'
import { UnifiedGeneratorContext, OutputStrategy, SortFieldDefinition, FilterFieldDefinition } from '@generators/strategies'
import { BaseGeneratorContext } from '@core/types'
import { TestUtils } from './test-utils'
import { DataModel, Enum } from '@zenstackhq/sdk/ast'
import { RelationField } from '@generators/unified/unified-relation-generator'
import { TypeKind } from '@utils/registry/base-registry'

export class MockFactory {
	static createMockSchemaComposer(): SchemaComposer {
		return new SchemaComposer()
	}

	static createMockTypeFormatter(): TypeFormatter {
		return TypeFormatter.fromOptions('PascalCase', 'camelCase')
	}

	static createMockSchemaProcessor(): SchemaProcessor {
		return new SchemaProcessor()
	}

	static createMockUnifiedTypeMapper(context: BaseGeneratorContext = TestUtils.createMockContext()): UnifiedTypeMapper {
		const typeFormatter = MockFactory.createMockTypeFormatter()
		return new UnifiedTypeMapper(typeFormatter, context.models, context.enums, context.options)
	}

	static createMockGraphQLRegistry(): GraphQLRegistry {
		const schemaComposer = MockFactory.createMockSchemaComposer()
		const registry = new GraphQLRegistry(schemaComposer)

		MockFactory.addEssentialScalarTypes(registry)

		registry['syncFromSchemaComposer']()

		return registry
	}

	private static addEssentialScalarTypes(registry: GraphQLRegistry): void {
		registry.addRelayRequirements()

		const schemaComposer = registry.schemaComposer

		MockFactory.addScalarToRegistry(registry, schemaComposer, 'DateTime', 'A date-time string at UTC')
		MockFactory.addScalarToRegistry(registry, schemaComposer, 'JSON', 'The `JSON` scalar type represents JSON values')
		MockFactory.addScalarToRegistry(registry, schemaComposer, 'Decimal', 'An arbitrary-precision Decimal type')

		MockFactory.addFilterInputTypes(schemaComposer)

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

	private static addScalarToRegistry(registry: GraphQLRegistry, schemaComposer: any, name: string, description: string): void {
		if (!schemaComposer.has(name)) {
			const scalarTC = schemaComposer.createScalarTC({
				name,
				description,
			})

			registry.registerType(name, TypeKind.SCALAR, scalarTC, true)
		}
	}

	private static addFilterInputTypes(schemaComposer: SchemaComposer): void {
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

	static createMockTypeScriptRegistry(): TypeScriptRegistry {
		return new TypeScriptRegistry()
	}

	static createMockGraphQLTypeFactories(): GraphQLTypeFactories {
		const schemaComposer = MockFactory.createMockSchemaComposer()
		const typeFormatter = MockFactory.createMockTypeFormatter()
		return new GraphQLTypeFactories(schemaComposer, typeFormatter)
	}

	static createMockOutputStrategy(): MockOutputStrategy {
		return new MockOutputStrategy()
	}

	static createMockUnifiedGeneratorContext(context: BaseGeneratorContext = TestUtils.createMockContext()): UnifiedGeneratorContext {
		return {
			outputStrategy: MockFactory.createMockOutputStrategy(),
			options: context.options,
			models: context.models,
			enums: context.enums,
			typeFormatter: MockFactory.createMockTypeFormatter(),
			attributeProcessor: MockFactory.createMockSchemaProcessor(),
			typeMapper: MockFactory.createMockUnifiedTypeMapper(context),
		}
	}

	static createMockGraphQLContext(context: BaseGeneratorContext = TestUtils.createMockContext()) {
		const typeFormatter = MockFactory.createMockTypeFormatter()
		const registry = MockFactory.createMockGraphQLRegistry()
		const schemaComposer = registry.schemaComposer

		return {
			...context,
			schemaComposer,
			attributeProcessor: MockFactory.createMockSchemaProcessor(),
			registry,
			typeFormatter,
			typeMapper: MockFactory.createMockUnifiedTypeMapper(context),
			typeFactories: new GraphQLTypeFactories(schemaComposer, typeFormatter),
		}
	}

	static createSpyOutputStrategy(): SpyOutputStrategy {
		return new SpyOutputStrategy()
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
		this.generatedTypes.push(typeName)
		return typeName
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
		this.generatedTypes.push('StringFilterInput', 'IntFilterInput', 'BooleanFilterInput')
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
