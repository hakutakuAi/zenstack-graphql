import { TypeScriptASTFactory } from '@utils/typescript/ast-factory'
import { DataModel } from '@zenstackhq/sdk/ast'
import { OutputStrategy, CommonTypeDefinition, SortFieldDefinition, FilterFieldDefinition } from './output-strategy'
import { RelationField } from '@generators/unified/unified-relation-generator'
import { COMMON_FILTER_TYPES, createFilterFields } from '@utils/filter-type-definitions'

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
		const sortInputName = typeName.endsWith('SortInput') ? typeName : `${typeName}SortInput`
		const sortFields = fields.length === 0 ? [{ name: '_placeholder', description: 'Placeholder field when no sortable fields are available' }] : fields

		this.astFactory.createSortInputType(sortInputName, sortFields)
		return sortInputName
	}

	createFilterInputType(typeName: string, fields: FilterFieldDefinition[]): string {
		const filterInputName = typeName.endsWith('FilterInput') ? typeName : `${typeName}FilterInput`

		const astFields = fields.map((field) => ({
			name: field.name,
			type: field.type,
			nullable: field.nullable ?? true,
		}))

		astFields.push({ name: 'AND', type: `[${filterInputName}!]`, nullable: true }, { name: 'OR', type: `[${filterInputName}!]`, nullable: true })

		this.astFactory.createFilterInputType(filterInputName, astFields)
		return filterInputName
	}

	createEmptyFilterInputType(typeName: string): string {
		const filterInputName = typeName.endsWith('FilterInput') ? typeName : `${typeName}FilterInput`

		const astFields = [
			{ name: 'AND', type: `[${filterInputName}!]`, nullable: true },
			{ name: 'OR', type: `[${filterInputName}!]`, nullable: true },
		]

		this.astFactory.createFilterInputType(filterInputName, astFields)
		return filterInputName
	}

	createEnumFilterInputType(enumName: string): string {
		const filterInputName = `${enumName}FilterInput`

		const astFields = [
			{ name: 'equals', type: enumName, nullable: true },
			{ name: 'not', type: enumName, nullable: true },
			{ name: 'in', type: `[${enumName}!]`, nullable: true },
			{ name: 'notIn', type: `[${enumName}!]`, nullable: true },
			{ name: 'AND', type: `[${filterInputName}!]`, nullable: true },
			{ name: 'OR', type: `[${filterInputName}!]`, nullable: true },
		]

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
		COMMON_FILTER_TYPES.forEach((definition) => {
			const fields = createFilterFields(definition)
			this.astFactory.createFilterInputType(definition.name, fields)
		})
	}

	createSortDirectionEnum(): void {
		this.astFactory.createSortDirectionEnum()
	}

	hasType(typeName: string): boolean {
		return this.astFactory.hasType(typeName)
	}

	createObjectType(typeName: string, fields: Record<string, any>, description?: string): string {
		this.astFactory.createObjectTypeFromFields(typeName, fields, description)
		return typeName
	}

	createInputType(typeName: string, model: DataModel, inputType: 'create' | 'update', description?: string): string {
		this.astFactory.createInputType(typeName, model, inputType, description)
		return typeName
	}

	createQueryArgsInputType(typeName: string, model: DataModel, description?: string): string {
		const queryArgsInputName = typeName

		const filterInputName = `${model.name}FilterInput`
		const sortInputName = `${model.name}SortInput`

		const fields: Array<{ name: string; type: string; nullable: boolean }> = []

		if (this.hasType(filterInputName)) {
			fields.push({ name: 'filter', type: filterInputName, nullable: true })
		}
		if (this.hasType(sortInputName)) {
			fields.push({ name: 'sort', type: sortInputName, nullable: true })
		}
		fields.push(
			{ name: 'first', type: 'Int', nullable: true },
			{ name: 'after', type: 'String', nullable: true },
			{ name: 'last', type: 'Int', nullable: true },
			{ name: 'before', type: 'String', nullable: true },
			{ name: 'connection', type: 'Boolean', nullable: true },
		)

		this.astFactory.createFilterInputType(queryArgsInputName, fields)
		return queryArgsInputName
	}

	processRelation(relation: RelationField): void {}

	getGeneratedTypeNames(filter?: (name: string) => boolean): string[] {
		return this.astFactory.getGeneratedTypeNames(filter)
	}

	getGeneratedCode(): string {
		return this.astFactory.getGeneratedCode()
	}
}
