import { DataModel, Enum } from '@zenstackhq/sdk/ast'
import { RelationField } from '@generators/unified/unified-relation-generator'
import { NormalizedOptions } from '@utils/config'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { SchemaProcessor } from '@utils/schema/schema-processor'
import { UnifiedTypeMapper } from '@utils/type-mapping/unified-type-mapper'

export interface OutputStrategy {
	createCommonTypes?(types: CommonTypeDefinition[]): void

	createSortInputType(typeName: string, fields: SortFieldDefinition[]): string

	createFilterInputType(typeName: string, fields: FilterFieldDefinition[]): string

	createEmptyFilterInputType(typeName: string): string

	createEnumFilterInputType(enumName: string): string

	createConnectionType(typeName: string): string

	createObjectType(typeName: string, fields: Record<string, any>, description?: string): string

	createInputType(typeName: string, model: DataModel, inputType: 'create' | 'update', description?: string): string

	createQueryArgsInputType(typeName: string, model: DataModel, description?: string): string

	createEnumType(enumType: Enum): string

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
	options: NormalizedOptions
	models: DataModel[]
	enums: Enum[]
	typeFormatter: TypeFormatter
	attributeProcessor: SchemaProcessor
	typeMapper?: UnifiedTypeMapper

	outputStrategy: OutputStrategy
}
