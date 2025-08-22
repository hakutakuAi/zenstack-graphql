import { DataModel } from '@zenstackhq/sdk/ast'
import { RelationField } from '@generators/unified/unified-relation-generator'

export interface OutputStrategy {
	createCommonTypes?(types: CommonTypeDefinition[]): void

	createSortInputType(typeName: string, fields: SortFieldDefinition[]): string

	createFilterInputType(typeName: string, fields: FilterFieldDefinition[]): string

	createConnectionType(typeName: string): string

	createObjectType(typeName: string, fields: Record<string, any>, description?: string): string

	createInputType(typeName: string, model: DataModel, inputType: 'create' | 'update', description?: string): string

	createPaginationTypes(): void

	createCommonFilterTypes(): void

	createSortDirectionEnum(): void

	processRelation(relation: RelationField): void

	hasProcessedRelation?(relationKey: string): boolean

	getProcessedRelations?(): string[]

	hasType(typeName: string): boolean

	getGeneratedTypeNames(filter?: (name: string) => boolean): string[]

	getGeneratedCode?(): string
}

export interface CommonTypeDefinition {
	name: string
	type: 'enum' | 'input' | 'object'
	definition: any
}

export interface SortFieldDefinition {
	name: string
	description?: string
}

export interface FilterFieldDefinition {
	name: string
	type: string
	nullable?: boolean
	description?: string
}

export interface UnifiedGeneratorContext {
	options: any
	models: DataModel[]
	enums: any[]
	typeFormatter: any
	attributeProcessor: any
	typeMapper?: any

	outputStrategy: OutputStrategy
}
