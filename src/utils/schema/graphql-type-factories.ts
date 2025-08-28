import { SchemaComposer, ObjectTypeComposer, InputTypeComposer, EnumTypeComposer } from 'graphql-compose'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { warning, ErrorCategory } from '@utils/error'
import { DataModel, Enum } from '@zenstackhq/sdk/ast'

export class GraphQLTypeFactories {
	private readonly schemaComposer: SchemaComposer<unknown>
	private readonly typeFormatter: TypeFormatter

	constructor(schemaComposer: SchemaComposer<unknown>, typeFormatter: TypeFormatter) {
		this.schemaComposer = schemaComposer
		this.typeFormatter = typeFormatter
	}

	createPageInfoType(): ObjectTypeComposer<any, any> {
		try {
			if (this.schemaComposer.has('PageInfo')) {
				return this.schemaComposer.getOTC('PageInfo')
			}

			const pageInfoTC = this.schemaComposer.createObjectTC({
				name: 'PageInfo',
				description: 'Information about pagination in a connection.',
				fields: {
					hasNextPage: {
						type: 'Boolean!',
						description: 'When paginating forwards, are there more items?',
					},
					hasPreviousPage: {
						type: 'Boolean!',
						description: 'When paginating backwards, are there more items?',
					},
					startCursor: {
						type: 'String',
						description: 'When paginating backwards, the cursor to continue.',
					},
					endCursor: {
						type: 'String',
						description: 'When paginating forwards, the cursor to continue.',
					},
				},
			})

			this.schemaComposer.set('PageInfo', pageInfoTC)
			return pageInfoTC
		} catch (error) {
			warning(`Failed to create PageInfo type: ${error instanceof Error ? error.message : String(error)}`, ErrorCategory.GENERATION, { error })

			const fallbackTC = this.schemaComposer.createObjectTC({
				name: 'PageInfo',
				description: 'Fallback PageInfo type due to error in creation',
				fields: {
					hasNextPage: 'Boolean!',
					hasPreviousPage: 'Boolean!',
				},
			})

			this.schemaComposer.set('PageInfo', fallbackTC)
			return fallbackTC
		}
	}

	createPaginationInputTypes(): InputTypeComposer<any>[] {
		try {
			const createdTypesTC: InputTypeComposer<any>[] = []

			if (!this.schemaComposer.has('ForwardPaginationInput')) {
				const forwardPaginationInputTC = this.schemaComposer.createInputTC({
					name: 'ForwardPaginationInput',
					description: 'Pagination input for forward pagination',
					fields: {
						first: {
							type: 'Int',
							description: 'Returns the first n elements from the list.',
						},
						after: {
							type: 'String',
							description: 'Returns the elements in the list that come after the specified cursor.',
						},
					},
				})

				this.schemaComposer.set('ForwardPaginationInput', forwardPaginationInputTC)
				createdTypesTC.push(forwardPaginationInputTC)
			} else {
				createdTypesTC.push(this.schemaComposer.getITC('ForwardPaginationInput'))
			}

			if (!this.schemaComposer.has('BackwardPaginationInput')) {
				const backwardPaginationInputTC = this.schemaComposer.createInputTC({
					name: 'BackwardPaginationInput',
					description: 'Pagination input for backward pagination',
					fields: {
						last: {
							type: 'Int',
							description: 'Returns the last n elements from the list.',
						},
						before: {
							type: 'String',
							description: 'Returns the elements in the list that come before the specified cursor.',
						},
					},
				})

				this.schemaComposer.set('BackwardPaginationInput', backwardPaginationInputTC)
				createdTypesTC.push(backwardPaginationInputTC)
			} else {
				createdTypesTC.push(this.schemaComposer.getITC('BackwardPaginationInput'))
			}

			if (!this.schemaComposer.has('PaginationInput')) {
				const paginationInputTC = this.schemaComposer.createInputTC({
					name: 'PaginationInput',
					description: 'Combined pagination input',
					fields: {
						first: {
							type: 'Int',
							description: 'Returns the first n elements from the list.',
						},
						after: {
							type: 'String',
							description: 'Returns the elements in the list that come after the specified cursor.',
						},
						last: {
							type: 'Int',
							description: 'Returns the last n elements from the list.',
						},
						before: {
							type: 'String',
							description: 'Returns the elements in the list that come before the specified cursor.',
						},
					},
				})

				this.schemaComposer.set('PaginationInput', paginationInputTC)
				createdTypesTC.push(paginationInputTC)
			} else {
				createdTypesTC.push(this.schemaComposer.getITC('PaginationInput'))
			}

			return createdTypesTC
		} catch (error) {
			warning(`Failed to create pagination input types: ${error instanceof Error ? error.message : String(error)}`, ErrorCategory.GENERATION, { error })
			return []
		}
	}

	createSortDirectionEnum(): EnumTypeComposer<any> {
		try {
			if (this.schemaComposer.has('SortDirection')) {
				return this.schemaComposer.getETC('SortDirection')
			}

			const sortDirectionTC = this.schemaComposer.createEnumTC({
				name: 'SortDirection',
				description: 'Sort direction for ordering results',
				values: {
					ASC: {
						value: 'ASC',
						description: 'Ascending order',
					},
					DESC: {
						value: 'DESC',
						description: 'Descending order',
					},
				},
			})

			this.schemaComposer.set('SortDirection', sortDirectionTC)
			return sortDirectionTC
		} catch (error) {
			warning(`Failed to create SortDirection enum: ${error instanceof Error ? error.message : String(error)}`, ErrorCategory.GENERATION, { error })

			const fallbackEnum = this.schemaComposer.createEnumTC({
				name: 'SortDirection',
				description: 'Sort direction (fallback due to error)',
				values: {
					ASC: { value: 'ASC' },
					DESC: { value: 'DESC' },
				},
			})

			this.schemaComposer.set('SortDirection', fallbackEnum)
			return fallbackEnum
		}
	}

	createConnectionType(modelType: string, description?: string): ObjectTypeComposer<any, any> {
		try {
			const connectionName = this.typeFormatter.formatConnectionTypeName(modelType)
			const edgeName = this.typeFormatter.formatEdgeTypeName(modelType)

			if (this.schemaComposer.has(connectionName)) {
				return this.schemaComposer.getOTC(connectionName)
			}

			this.createEdgeType(modelType)

			const connectionTC = this.schemaComposer.createObjectTC({
				name: connectionName,
				description: description || `A connection to a list of ${modelType} items.`,
				interfaces: ['Connection'],
				fields: {
					pageInfo: {
						type: 'PageInfo!',
						description: 'Information to aid in pagination.',
					},
					edges: {
						type: `[${edgeName}!]!`,
						description: `A list of ${modelType} edges.`,
					},
					totalCount: {
						type: 'Int!',
						description: 'The total count of items in the connection.',
					},
				},
			})

			this.schemaComposer.set(connectionName, connectionTC)
			return connectionTC
		} catch (error) {
			const fallbackConnectionName = this.typeFormatter.formatConnectionTypeName(modelType)
			const fallbackConnection = this.schemaComposer.createObjectTC({
				name: fallbackConnectionName,
				description: `Fallback connection for ${modelType} due to error`,
				fields: {
					nodes: `[${modelType}!]!`,
				},
			})

			this.schemaComposer.set(fallbackConnectionName, fallbackConnection)
			return fallbackConnection
		}
	}

	createEdgeType(modelType: string): ObjectTypeComposer<any, any> {
		try {
			const edgeName = this.typeFormatter.formatEdgeTypeName(modelType)

			if (this.schemaComposer.has(edgeName)) {
				return this.schemaComposer.getOTC(edgeName)
			}

			const edgeTC = this.schemaComposer.createObjectTC({
				name: edgeName,
				description: `An edge in a ${modelType} connection.`,
				interfaces: ['Edge'],
				fields: {
					node: {
						type: `${modelType}!`,
						description: `The ${modelType} at the end of the edge.`,
					},
					cursor: {
						type: 'String!',
						description: 'A cursor for use in pagination.',
					},
				},
			})

			this.schemaComposer.set(edgeName, edgeTC)
			return edgeTC
		} catch (error) {
			const fallbackEdgeName = this.typeFormatter.formatEdgeTypeName(modelType)
			const fallbackEdge = this.schemaComposer.createObjectTC({
				name: fallbackEdgeName,
				description: `Fallback edge for ${modelType} due to error`,
				fields: {
					node: `${modelType}!`,
				},
			})

			this.schemaComposer.set(fallbackEdgeName, fallbackEdge)
			return fallbackEdge
		}
	}

	createSortInputType(modelType: string, fields: Record<string, { description: string }>): InputTypeComposer<any> {
		try {
			this.createSortDirectionEnum()

			const sortInputName = this.typeFormatter.formatSortInputTypeName(modelType)

			if (this.schemaComposer.has(sortInputName)) {
				return this.schemaComposer.getITC(sortInputName)
			}

			const sortFields: Record<string, { type: string; description: string }> = {}

			for (const [fieldName, fieldConfig] of Object.entries(fields)) {
				sortFields[fieldName] = {
					type: 'SortDirection',
					description: fieldConfig.description || `Sort by ${fieldName}`,
				}
			}

			const sortInputTC = this.schemaComposer.createInputTC({
				name: sortInputName,
				description: `Sort input for ${modelType} connections`,
				fields:
					Object.keys(sortFields).length > 0
						? sortFields
						: {
								_placeholder: {
									type: 'String',
									description: 'Placeholder field when no sortable fields are available',
								},
							},
			})

			this.schemaComposer.set(sortInputName, sortInputTC)
			return sortInputTC
		} catch (error) {
			const fallbackSortInputName = this.typeFormatter.formatSortInputTypeName(modelType)
			const fallbackSortInput = this.schemaComposer.createInputTC({
				name: fallbackSortInputName,
				description: `Fallback sort input for ${modelType} due to error`,
				fields: {
					_placeholder: {
						type: 'String',
						description: 'Placeholder field when no sortable fields are available',
					},
				},
			})

			this.schemaComposer.set(fallbackSortInputName, fallbackSortInput)
			return fallbackSortInput
		}
	}

	createFilterInputType(model: DataModel, modelType: string): InputTypeComposer<any> {
		try {
			const filterInputName = this.typeFormatter.formatFilterInputTypeName(modelType)

			if (this.schemaComposer.has(filterInputName)) {
				return this.schemaComposer.getITC(filterInputName)
			}

			const fields: Record<string, any> = {}

			for (const field of model.fields) {
				const fieldName = this.typeFormatter.formatFieldName(field.name)
				fields[fieldName] = {
					type: 'String',
					description: `Filter by ${fieldName}`,
				}
			}

			const filterInputTC = this.schemaComposer.createInputTC({
				name: filterInputName,
				description: `Filter input for ${modelType}`,
				fields:
					Object.keys(fields).length > 0
						? fields
						: {
								_placeholder: {
									type: 'String',
									description: 'Placeholder field when no filterable fields are available',
								},
							},
			})

			this.schemaComposer.set(filterInputName, filterInputTC)
			return filterInputTC
		} catch (error) {
			const fallbackFilterInputName = this.typeFormatter.formatFilterInputTypeName(modelType)
			const fallbackFilterInput = this.schemaComposer.createInputTC({
				name: fallbackFilterInputName,
				description: `Fallback filter input for ${modelType} due to error`,
				fields: {
					_placeholder: {
						type: 'String',
						description: 'Placeholder field when no filterable fields are available',
					},
				},
			})

			this.schemaComposer.set(fallbackFilterInputName, fallbackFilterInput)
			return fallbackFilterInput
		}
	}

	createInputType(model: DataModel, modelType: string): InputTypeComposer<any> {
		try {
			const inputName = `${modelType}Input`

			if (this.schemaComposer.has(inputName)) {
				return this.schemaComposer.getITC(inputName)
			}

			const fields: Record<string, any> = {}

			for (const field of model.fields) {
				const fieldName = this.typeFormatter.formatFieldName(field.name)
				fields[fieldName] = {
					type: 'String',
					description: `Input for ${fieldName}`,
				}
			}

			const inputTC = this.schemaComposer.createInputTC({
				name: inputName,
				description: `Input type for ${modelType}`,
				fields:
					Object.keys(fields).length > 0
						? fields
						: {
								_placeholder: {
									type: 'String',
									description: 'Placeholder field when no input fields are available',
								},
							},
			})

			this.schemaComposer.set(inputName, inputTC)
			return inputTC
		} catch (error) {
			const fallbackInputName = `${modelType}Input`
			const fallbackInput = this.schemaComposer.createInputTC({
				name: fallbackInputName,
				description: `Fallback input for ${modelType} due to error`,
				fields: {
					_placeholder: {
						type: 'String',
						description: 'Placeholder field when no input fields are available',
					},
				},
			})

			this.schemaComposer.set(fallbackInputName, fallbackInput)
			return fallbackInput
		}
	}

	createEnumType(enumType: Enum): EnumTypeComposer<any> {
		try {
			const enumName = this.typeFormatter.formatTypeName(enumType.name)

			if (this.schemaComposer.has(enumName)) {
				return this.schemaComposer.getETC(enumName)
			}

			const values: Record<string, any> = {}
			for (const value of enumType.fields) {
				values[value.name] = {
					value: value.name,
					description: `${value.name} value`,
				}
			}

			const enumTC = this.schemaComposer.createEnumTC({
				name: enumName,
				description: `${enumName} enum`,
				values,
			})

			this.schemaComposer.set(enumName, enumTC)
			return enumTC
		} catch (error) {
			const fallbackEnumName = this.typeFormatter.formatTypeName(enumType.name)
			const fallbackEnum = this.schemaComposer.createEnumTC({
				name: fallbackEnumName,
				description: `Fallback enum for ${enumType.name} due to error`,
				values: {
					UNKNOWN: { value: 'UNKNOWN' },
				},
			})

			this.schemaComposer.set(fallbackEnumName, fallbackEnum)
			return fallbackEnum
		}
	}
}
