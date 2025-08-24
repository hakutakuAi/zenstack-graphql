import { DataModel, Enum, DataModelField } from '@zenstackhq/sdk/ast'
import { OutputStrategy, SortFieldDefinition, FilterFieldDefinition } from '@generators/strategies'
import { RelationField } from '@generators/unified/unified-relation-generator'

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
