import { TypeScriptASTFactory } from '@utils/typescript/ast-factory'
import { DataModel } from '@zenstackhq/sdk/ast'
import { OutputStrategy, CommonTypeDefinition, SortFieldDefinition, FilterFieldDefinition } from './output-strategy'
import { RelationField } from '@generators/unified/unified-relation-generator'

export class TypeScriptOutputStrategy implements OutputStrategy {
	constructor(private readonly astFactory: TypeScriptASTFactory) {}

	createCommonTypes(types: CommonTypeDefinition[]): void {
		types.forEach((type) => {
			switch (type.type) {
				case 'enum':
					this.astFactory.createEnumType(type.definition)
					break
				case 'input':
					break
			}
		})
	}

	createEnumType(enumType: any): string {
		this.astFactory.createEnumType(enumType)
		return enumType.name
	}

	createSortInputType(typeName: string, fields: SortFieldDefinition[]): string {
		const sortInputName = `${typeName}SortInput`

		const sortFields = fields.length === 0 ? [{ name: '_placeholder', description: 'Placeholder field when no sortable fields are available' }] : fields

		this.astFactory.createSortInputType(typeName, sortFields)
		return sortInputName
	}

	createFilterInputType(typeName: string, fields: FilterFieldDefinition[]): string {
		const filterInputName = `${typeName}FilterInput`

		const astFields = fields.map((field) => ({
			name: field.name,
			type: field.type,
			nullable: field.nullable ?? true,
		}))

		astFields.push({ name: 'AND', type: `[${filterInputName}!]`, nullable: true }, { name: 'OR', type: `[${filterInputName}!]`, nullable: true })

		this.astFactory.createFilterInputType(filterInputName, astFields)
		return filterInputName
	}

	createConnectionType(typeName: string): string {
		const connectionName = `${typeName}Connection`
		this.astFactory.createConnectionType(typeName)
		return connectionName
	}

	createPaginationTypes(): void {
		this.astFactory.createPaginationInputTypes()
	}

	createCommonFilterTypes(): void {
		this.astFactory.createFilterInputType('NumericFilterInput', [
			{ name: 'equals', type: 'Float', nullable: true },
			{ name: 'not', type: 'Float', nullable: true },
			{ name: 'gt', type: 'Float', nullable: true },
			{ name: 'lt', type: 'Float', nullable: true },
		])

		this.astFactory.createFilterInputType('DateTimeFilterInput', [
			{ name: 'equals', type: 'Date', nullable: true },
			{ name: 'not', type: 'Date', nullable: true },
			{ name: 'gt', type: 'Date', nullable: true },
			{ name: 'lt', type: 'Date', nullable: true },
		])

		this.astFactory.createFilterInputType('StringFilterInput', [
			{ name: 'equals', type: 'String', nullable: true },
			{ name: 'not', type: 'String', nullable: true },
			{ name: 'in', type: '[String!]', nullable: true },
			{ name: 'notIn', type: '[String!]', nullable: true },
			{ name: 'contains', type: 'String', nullable: true },
			{ name: 'startsWith', type: 'String', nullable: true },
			{ name: 'endsWith', type: 'String', nullable: true },
		])

		this.astFactory.createFilterInputType('BooleanFilterInput', [
			{ name: 'equals', type: 'Boolean', nullable: true },
			{ name: 'not', type: 'Boolean', nullable: true },
		])
	}

	createSortDirectionEnum(): void {
		this.astFactory.createSortDirectionEnum()
	}

	hasType(typeName: string): boolean {
		return false
	}

	createObjectType(typeName: string, fields: Record<string, any>, description?: string): string {
		this.astFactory.createObjectTypeFromFields(typeName, fields, description)
		return typeName
	}

	createInputType(typeName: string, model: DataModel, inputType: 'create' | 'update', description?: string): string {
		return ''
	}

	processRelation(relation: RelationField): void {}

	getGeneratedTypeNames(filter?: (name: string) => boolean): string[] {
		return this.astFactory.getGeneratedTypeNames(filter)
	}

	getGeneratedCode(): string {
		return this.astFactory.getGeneratedCode()
	}
}
