import { SchemaComposer } from 'graphql-compose'
import { GraphQLRegistry } from '@utils/registry'
import { GraphQLTypeFactories } from '@utils/schema/graphql-type-factories'
import { TypeKind } from '@utils/registry'
import { ErrorCategory, PluginError } from '@utils/error'
import { DataModel } from '@zenstackhq/sdk/ast'
import { OutputStrategy, CommonTypeDefinition, SortFieldDefinition, FilterFieldDefinition } from './output-strategy'
import { RelationField } from '@generators/unified/unified-relation-generator'
import { COMMON_FILTER_TYPES, createGraphQLFilterFields } from '@utils/filter-type-definitions'

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
		COMMON_FILTER_TYPES.forEach((definition) => {
			let actualDefinition = definition
			
			if (definition.type === 'datetime') {
				const dateTimeType = this.options.scalarTypes?.['DateTime'] || 'DateTime'
				actualDefinition = {
					...definition,
					name: `${dateTimeType}FilterInput`,
					baseType: dateTimeType,
				}
			}

			if (!this.schemaComposer.has(actualDefinition.name)) {
				const fields = createGraphQLFilterFields(actualDefinition)
				const filterTC = this.schemaComposer.createInputTC({
					name: actualDefinition.name,
					description: `Input type for ${actualDefinition.type} filtering operations`,
					fields,
				})
				this.registry.registerType(actualDefinition.name, TypeKind.INPUT, filterTC, true)
			}
		})
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
		const generatedTypes = this.registry.getGeneratedTypes()
		const typeNames = generatedTypes.map(type => type.name)
		return filter ? typeNames.filter(filter) : typeNames
	}

}
