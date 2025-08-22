import { SchemaComposer } from 'graphql-compose'
import { GraphQLRegistry } from '@utils/registry'
import { GraphQLTypeFactories } from '@utils/schema/graphql-type-factories'
import { TypeKind } from '@utils/registry'
import { ErrorCategory, PluginError } from '@utils/error'
import { DataModel } from '@zenstackhq/sdk/ast'
import { OutputStrategy, CommonTypeDefinition, SortFieldDefinition, FilterFieldDefinition } from './output-strategy'
import { RelationField } from '@generators/unified/unified-relation-generator'

export class GraphQLOutputStrategy implements OutputStrategy {
	constructor(
		private readonly registry: GraphQLRegistry,
		private readonly schemaComposer: SchemaComposer<unknown>,
		private readonly typeFactories: GraphQLTypeFactories,
		private readonly options: any,
	) {}

	createCommonTypes(types: CommonTypeDefinition[]): void {
		types.forEach((type) => {
			switch (type.type) {
				case 'enum':
					if (!this.hasType(type.name)) {
						const enumTC = this.schemaComposer.createEnumTC(type.definition)
						this.registry.registerType(type.name, TypeKind.ENUM, enumTC, true)
					}
					break
				case 'input':
					if (!this.hasType(type.name)) {
						const inputTC = this.schemaComposer.createInputTC(type.definition)
						this.registry.registerType(type.name, TypeKind.INPUT, inputTC, true)
					}
					break
			}
		})
	}

	createEnumType(enumType: any): string {
		const enumName = enumType.name

		if (this.hasType(enumName)) {
			return enumName
		}

		const enumValues = enumType.fields.reduce((acc: any, field: any) => {
			acc[field.name] = { value: field.name }
			return acc
		}, {})

		const enumTC = this.schemaComposer.createEnumTC({
			name: enumName,
			description: enumType.comments?.[0],
			values: enumValues,
		})

		this.registry.registerType(enumName, TypeKind.ENUM, enumTC, true)
		return enumName
	}

	createSortInputType(typeName: string, fields: SortFieldDefinition[]): string {
		const sortInputName = `${typeName}SortInput`

		if (this.schemaComposer.has(sortInputName)) {
			return sortInputName
		}

		const fieldsMap = fields.reduce(
			(acc, field) => {
				acc[field.name] = { description: field.description || `Sort by ${field.name}` }
				return acc
			},
			{} as Record<string, { description: string }>,
		)

		try {
			const sortInputTC = this.typeFactories.createSortInputType(typeName, fieldsMap)
			this.registry.registerType(sortInputName, TypeKind.INPUT, sortInputTC, true)
			return sortInputName
		} catch (error) {
			throw new PluginError(`Error creating sort input type for ${typeName}`, ErrorCategory.GENERATION, { typeName, error }, [
				'Check if the type fields are defined correctly',
			])
		}
	}

	createFilterInputType(typeName: string, fields: FilterFieldDefinition[]): string {
		const filterInputName = `${typeName}FilterInput`

		if (this.schemaComposer.has(filterInputName)) {
			return filterInputName
		}

		const fieldsMap: Record<string, { type: string; description: string }> = {}

		fields.forEach((field) => {
			fieldsMap[field.name] = {
				type: field.type,
				description: field.description || `Filter by ${field.name}`,
			}
		})

		if (Object.keys(fieldsMap).length === 0) {
			return filterInputName
		}

		fieldsMap.AND = {
			type: `[${filterInputName}!]`,
			description: 'Logical AND operation',
		}
		fieldsMap.OR = {
			type: `[${filterInputName}!]`,
			description: 'Logical OR operation',
		}

		const filterInputTC = this.schemaComposer.createInputTC({
			name: filterInputName,
			description: `Filter input type for ${typeName}`,
			fields: fieldsMap,
		})

		this.registry.registerType(filterInputName, TypeKind.INPUT, filterInputTC, true)
		return filterInputName
	}

	createConnectionType(typeName: string): string {
		const connectionName = `${typeName}Connection`

		if (this.schemaComposer.has(connectionName)) {
			return connectionName
		}

		try {
			const connectionTC = this.typeFactories.createConnectionType(typeName)
			this.registry.registerType(connectionName, TypeKind.OBJECT, connectionTC, true)
			return connectionName
		} catch (error) {
			throw new PluginError(`Error creating connection type for ${typeName}`, ErrorCategory.GENERATION, { typeName, error }, [
				'Check if the base type is defined correctly',
			])
		}
	}

	createPaginationTypes(): void {
		if (!this.hasType('PageInfo')) {
			const pageInfoTC = this.typeFactories.createPageInfoType()
			this.registry.registerType('PageInfo', TypeKind.OBJECT, pageInfoTC, true)
		}

		const paginationInputTypes = this.typeFactories.createPaginationInputTypes()
		paginationInputTypes.forEach((inputTC) => {
			if (!this.hasType(inputTC.getTypeName())) {
				this.registry.registerType(inputTC.getTypeName(), TypeKind.INPUT, inputTC, true)
			}
		})
	}

	createCommonFilterTypes(): void {
		this.createFilterType('NumericFilterInput', 'numeric', 'Float', false)

		const dateTimeType = this.options.scalarTypes['DateTime'] || 'DateTime'
		this.createFilterType(`${dateTimeType}FilterInput`, 'datetime', dateTimeType, false)

		this.createFilterType('StringFilterInput', 'string', 'String', true)
		this.createFilterType('BooleanFilterInput', 'boolean', 'Boolean', false)
	}

	createSortDirectionEnum(): void {
		if (this.registry.isTypeOfKind('SortDirection', TypeKind.ENUM)) {
			return
		}

		try {
			const sortDirectionTC = this.typeFactories.createSortDirectionEnum()
			this.registry.registerType('SortDirection', TypeKind.ENUM, sortDirectionTC, true)
		} catch (error) {
			throw new PluginError(`Failed to create SortDirection enum`, ErrorCategory.GENERATION, { originalError: error }, [
				'Check if SortDirection enum is already defined elsewhere',
			])
		}
	}

	hasType(typeName: string): boolean {
		return this.schemaComposer.has(typeName) || this.registry.hasType(typeName)
	}

	createObjectType(typeName: string, fields: Record<string, any>, description?: string): string {
		if (this.hasType(typeName)) {
			return typeName
		}

		try {
			const objectTC = this.schemaComposer.createObjectTC({
				name: typeName,
				description,
				fields,
			})

			this.registry.registerType(typeName, TypeKind.OBJECT, objectTC, true)
			return typeName
		} catch (error) {
			throw new PluginError(`Error creating object type ${typeName}`, ErrorCategory.GENERATION, { typeName, error })
		}
	}

	createInputType(typeName: string, model: DataModel, inputType: 'create' | 'update', description?: string): string {
		return ''
	}

	createQueryArgsInputType(typeName: string, model: DataModel, description?: string): string {
		const queryArgsInputName = typeName

		if (this.hasType(queryArgsInputName)) {
			return queryArgsInputName
		}

		const filterInputName = `${model.name}FilterInput`
		const sortInputName = `${model.name}SortInput`

		const fields: Record<string, { type: string; description: string }> = {}

		if (this.hasType(filterInputName)) {
			fields.filter = {
				type: filterInputName,
				description: `Filter conditions for ${model.name}`,
			}
		}

		if (this.hasType(sortInputName)) {
			fields.sort = {
				type: sortInputName,
				description: `Sort options for ${model.name}`,
			}
		}

		fields.first = {
			type: 'Int',
			description: 'Number of items to return from the beginning',
		}
		fields.after = {
			type: 'String',
			description: 'Cursor for pagination after this item',
		}
		fields.last = {
			type: 'Int',
			description: 'Number of items to return from the end',
		}
		fields.before = {
			type: 'String',
			description: 'Cursor for pagination before this item',
		}
		fields.connection = {
			type: 'Boolean',
			description: 'Return connection format with edges and pageInfo',
		}

		try {
			const queryArgsInputTC = this.schemaComposer.createInputTC({
				name: queryArgsInputName,
				description: description || `Query arguments for ${model.name}`,
				fields,
			})

			this.registry.registerType(queryArgsInputName, TypeKind.INPUT, queryArgsInputTC, true)
			return queryArgsInputName
		} catch (error) {
			throw new PluginError(`Error creating query args input type for ${model.name}`, ErrorCategory.GENERATION, { typeName, error }, [
				'Check if the filter and sort input types are defined correctly',
			])
		}
	}

	processRelation(relation: RelationField): void {}

	hasProcessedRelation(relationKey: string): boolean {
		return this.registry.hasProcessedRelation(relationKey)
	}

	getProcessedRelations(): string[] {
		return this.registry.getProcessedRelations()
	}

	getGeneratedTypeNames(filter?: (name: string) => boolean): string[] {
		const typeNames = this.registry.getTypeNamesByKind(TypeKind.INPUT)
		return filter ? typeNames.filter(filter) : typeNames
	}

	private createFilterType(name: string, description: string, typeName: string, includeStringOperations: boolean): void {
		if (this.schemaComposer.has(name)) {
			return
		}

		const fields: Record<string, { type: string; description: string }> = {
			equals: {
				type: typeName,
				description: 'Equal to the given value',
			},
			not: {
				type: typeName,
				description: 'Not equal to the given value',
			},
		}

		if (typeName !== 'Boolean' && typeName !== 'String') {
			fields.gt = {
				type: typeName,
				description: 'Greater than the given value',
			}
			fields.lt = {
				type: typeName,
				description: 'Less than the given value',
			}
		}

		if (includeStringOperations) {
			fields.in = {
				type: `[${typeName}!]`,
				description: 'In the given list of values',
			}
			fields.notIn = {
				type: `[${typeName}!]`,
				description: 'Not in the given list of values',
			}
			fields.contains = {
				type: typeName,
				description: 'Contains the given value',
			}
			fields.startsWith = {
				type: typeName,
				description: 'Starts with the given value',
			}
			fields.endsWith = {
				type: typeName,
				description: 'Ends with the given value',
			}
		}

		const filterTC = this.schemaComposer.createInputTC({
			name,
			description: `Input type for ${description} filtering operations`,
			fields,
		})

		this.registry.registerType(name, TypeKind.INPUT, filterTC, true)
	}
}
