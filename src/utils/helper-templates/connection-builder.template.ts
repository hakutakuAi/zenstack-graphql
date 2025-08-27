export const CONNECTION_BUILDER_TEMPLATE = `export class ConnectionBuilder {
	/**
	 * Generic connection builder that handles both regular models and composite key models
	 */
	static async build<T>(args: {
		prisma: PrismaClient
		model: string
		pagination: PaginationArgs
		where?: any
		orderBy?: any
		include?: any
		info?: GraphQLResolveInfo
		relationFields?: string[]
		cursorField?: string
		hasIdField?: boolean
	}): Promise<ConnectionResult<T>> {
		const { 
			prisma, 
			model, 
			pagination, 
			where, 
			orderBy, 
			include, 
			info, 
			relationFields = [],
			cursorField = 'id',
			hasIdField = true
		} = args
		const { first, after, last, before } = pagination

		// Calculate pagination parameters
		let take = first || last || 10
		if (last) take = -take

		// For composite key models, we skip cursor-based pagination
		const cursor = (hasIdField && (after || before)) 
			? { [cursorField]: (after || before)! } 
			: undefined
		const skip = cursor ? 1 : 0

		// Build include from GraphQL selection if info is provided
		const finalInclude = info ? buildPrismaInclude(info, relationFields) : include

		// Get model delegate
		const modelDelegate = (prisma as any)[model.toLowerCase()]

		// Prepare query options
		const queryOptions: any = {
			take: Math.abs(take) + 1, // Get one extra to check for next page
			where,
			orderBy,
			include: finalInclude,
		}

		// Only add cursor and skip for models with ID field
		if (hasIdField && cursor) {
			queryOptions.cursor = cursor
			queryOptions.skip = skip
		}

		// Fetch items
		const items = await modelDelegate.findMany(queryOptions)

		// Calculate totalCount
		const totalCount = await modelDelegate.count({ where })

		// Determine pagination info
		const hasNextPage = first ? items.length > first : false
		const hasPreviousPage = last ? items.length > Math.abs(last) : false

		// Remove extra item if present
		const resultItems = hasNextPage || hasPreviousPage ? items.slice(0, -1) : items

		// Build edges - use composite key for cursor if no ID field
		const edges = resultItems.map((item: any, index: number) => {
			let cursor: string
			if (hasIdField && item[cursorField]) {
				cursor = item[cursorField]
			} else {
				// For composite key models, create a cursor from available fields or use index
				cursor = item.postId && item.categoryId 
					? \`\${item.postId}:\${item.categoryId}\`
					: String(index)
			}
			
			return {
				node: item,
				cursor,
			}
		})

		return {
			pageInfo: {
				hasNextPage,
				hasPreviousPage,
				startCursor: edges[0]?.cursor,
				endCursor: edges[edges.length - 1]?.cursor,
			},
			edges,
			totalCount,
		}
	}

	{{MODEL_SPECIFIC_METHODS}}
}`

export const MODEL_CONNECTION_METHOD_TEMPLATE = `
	static async build{{MODEL_NAME}}Connection(
		prisma: PrismaClient,
		args: {{MODEL_NAME}}QueryArgs,
		info?: GraphQLResolveInfo
	): Promise<{{MODEL_NAME}}Connection> {
		{{FILTER_SORT_LOGIC}}
		const include = info ? FieldSelection.build{{MODEL_NAME}}Include(info) : {{MODEL_NAME_UPPER}}_INCLUDES
		
		return this.build<{{MODEL_NAME}}>({
			prisma,
			model: '{{PRISMA_MODEL_NAME}}',
			pagination: {
				first: args.first,
				after: args.after,
				last: args.last,
				before: args.before,
			},
			where,
			orderBy,
			include,
			info,
			relationFields: [{{RELATION_FIELDS}}],
			hasIdField: {{HAS_ID_FIELD}},
			cursorField: '{{CURSOR_FIELD}}',
		})
	}`