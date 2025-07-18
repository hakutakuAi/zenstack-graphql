import { SchemaComposer, ObjectTypeComposer, InputTypeComposer, EnumTypeComposer } from 'graphql-compose'
import { ErrorHandler } from '@utils/error/error-handler'
import { SchemaOp } from '@utils/error'
import { TypeFormatter } from '@utils/schema/type-formatter'

export class GraphQLTypeFactories {
	private readonly schemaComposer: SchemaComposer<unknown>
	private readonly errorHandler: ErrorHandler
	private readonly typeFormatter: TypeFormatter

	constructor(schemaComposer: SchemaComposer<unknown>, errorHandler: ErrorHandler, typeFormatter: TypeFormatter) {
		this.schemaComposer = schemaComposer
		this.errorHandler = errorHandler
		this.typeFormatter = typeFormatter
	}

	@SchemaOp({
		suggestions: ['Check GraphQL type naming conflicts', 'Ensure PageInfo fields are valid GraphQL types']
	})
	createPageInfoType(): ObjectTypeComposer<any, any> {
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
	}

	@SchemaOp({
		suggestions: ['Check GraphQL input type naming conflicts', 'Ensure input fields are valid GraphQL types']
	})
	createPaginationInputTypes(): InputTypeComposer<any>[] {
		const createdTypesTC = []

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
	}

	@SchemaOp({
		suggestions: ['Check GraphQL enum naming conflicts']
	})
	createSortDirectionEnum(): EnumTypeComposer<any> {
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
	}

	@SchemaOp({
		suggestions: ['Check model definition in schema', 'Verify all field types are supported for connection types']
	})
	createConnectionType(modelType: string, description?: string): ObjectTypeComposer<any, any> {
		const connectionName = this.typeFormatter.formatConnectionTypeName(modelType)
		const edgeName = this.typeFormatter.formatEdgeTypeName(modelType)

		if (this.schemaComposer.has(connectionName)) {
			return this.schemaComposer.getOTC(connectionName)
		}

		this.createEdgeType(modelType)

		const connectionTC = this.schemaComposer.createObjectTC({
			name: connectionName,
			description: description || `A connection to a list of ${modelType} items.`,
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
	}

	@SchemaOp({
		suggestions: ['Check for naming conflicts', 'Ensure node type is defined in the schema']
	})
	createEdgeType(modelType: string): ObjectTypeComposer<any, any> {
		const edgeName = this.typeFormatter.formatEdgeTypeName(modelType)

		if (this.schemaComposer.has(edgeName)) {
			return this.schemaComposer.getOTC(edgeName)
		}

		const edgeTC = this.schemaComposer.createObjectTC({
			name: edgeName,
			description: `An edge in a ${modelType} connection.`,
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
	}

	@SchemaOp({
		suggestions: ['Check field definitions in the model', 'Ensure fields are valid for sorting']
	})
	createSortInputType(modelType: string, fields: Record<string, { description: string }>): InputTypeComposer<any> {
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
			fields: Object.keys(sortFields).length > 0 ? sortFields : {
				_placeholder: {
					type: 'String',
					description: 'Placeholder field when no sortable fields are available'
				}
			},
		})

		this.schemaComposer.set(sortInputName, sortInputTC)
		return sortInputTC
	}
}

export function createGraphQLTypeFactories(schemaComposer: SchemaComposer<unknown>, errorHandler: ErrorHandler, typeFormatter: TypeFormatter): GraphQLTypeFactories {
	return new GraphQLTypeFactories(schemaComposer, errorHandler, typeFormatter)
}
