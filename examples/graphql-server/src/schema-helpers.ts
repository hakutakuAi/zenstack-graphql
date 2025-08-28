import type { GraphQLResolveInfo } from 'graphql'
import { User, UserQueryArgs, UserConnection, UserFilterInput, UserSortInput, Post, PostQueryArgs, PostConnection, PostFilterInput, PostSortInput, Category, CategoryQueryArgs, CategoryConnection, CategoryFilterInput, PostCategory, PostCategoryQueryArgs, PostCategoryConnection, PostCategoryFilterInput, Comment, CommentQueryArgs, CommentConnection, CommentFilterInput, CommentSortInput } from './schema'

export interface PaginationArgs {
	first?: number
	after?: string
	last?: number
	before?: string
}

export interface ConnectionResult<T> {
	pageInfo: {
		hasNextPage: boolean
		hasPreviousPage: boolean
		startCursor?: string
		endCursor?: string
	}
	edges: Array<{
		node: T
		cursor: string
	}>
	totalCount: number
}

export interface ConnectionConfig {
	findManyOptions: {
		take: number
		where?: any
		orderBy?: any
		include?: any
		cursor?: any
		skip?: number
	}
	countOptions: {
		where?: any
	}
	paginationInfo: {
		first?: number
		last?: number
		after?: string
		before?: string
		cursorField: string
		hasIdField: boolean
		relationFields: string[]
	}
}

export class ConnectionBuilder {
	/**
	 * Build connection configuration without executing queries
	 */
	static buildConfig(args: {
		pagination: PaginationArgs
		where?: any
		orderBy?: any
		include?: any
		info?: any
		relationFields?: string[]
		cursorField?: string
		hasIdField?: boolean
	}): ConnectionConfig {
		const { 
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

		// Prepare query options
		const findManyOptions: any = {
			take: Math.abs(take) + 1, // Get one extra to check for next page
			where,
			orderBy,
			include: finalInclude,
		}

		// Only add cursor and skip for models with ID field
		if (hasIdField && cursor) {
			findManyOptions.cursor = cursor
			findManyOptions.skip = skip
		}

		return {
			findManyOptions,
			countOptions: { where },
			paginationInfo: {
				first,
				last,
				after,
				before,
				cursorField,
				hasIdField,
				relationFields
			}
		}
	}

	/**
	 * Process query results into connection format
	 */
	static processResults<T>(
		items: T[],
		totalCount: number,
		paginationInfo: ConnectionConfig['paginationInfo']
	): ConnectionResult<T> {
		const { first, last, cursorField, hasIdField } = paginationInfo

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
					? `${item.postId}:${item.categoryId}`
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


	
	static buildUserConnectionConfig(
		args: UserQueryArgs,
		info?: any
	): ConnectionConfig {
		const where = 'filter' in args ? FilterBuilder.buildUserFilter((args as any).filter) : {}
		const orderBy = 'sort' in args ? SortBuilder.buildUserSort((args as any).sort) : undefined
		const include = info ? FieldSelection.buildUserInclude(info) : USER_INCLUDES
		
		return this.buildConfig({
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
			relationFields: ['posts', 'comments'],
			hasIdField: true,
			cursorField: 'id',
		})
	}

	static buildPostConnectionConfig(
		args: PostQueryArgs,
		info?: any
	): ConnectionConfig {
		const where = 'filter' in args ? FilterBuilder.buildPostFilter((args as any).filter) : {}
		const orderBy = 'sort' in args ? SortBuilder.buildPostSort((args as any).sort) : undefined
		const include = info ? FieldSelection.buildPostInclude(info) : POST_INCLUDES
		
		return this.buildConfig({
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
			relationFields: ['author', 'categories', 'comments'],
			hasIdField: true,
			cursorField: 'id',
		})
	}

	static buildCategoryConnectionConfig(
		args: CategoryQueryArgs,
		info?: any
	): ConnectionConfig {
		const where = 'filter' in args ? FilterBuilder.buildCategoryFilter((args as any).filter) : {}
		const orderBy = undefined
		const include = info ? FieldSelection.buildCategoryInclude(info) : CATEGORY_INCLUDES
		
		return this.buildConfig({
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
			relationFields: ['posts'],
			hasIdField: true,
			cursorField: 'id',
		})
	}

	static buildPostCategoryConnectionConfig(
		args: PostCategoryQueryArgs,
		info?: any
	): ConnectionConfig {
		const where = {}
		const orderBy = undefined
		const include = info ? FieldSelection.buildPostCategoryInclude(info) : POSTCATEGORY_INCLUDES
		
		return this.buildConfig({
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
			relationFields: ['post', 'category'],
			hasIdField: false,
			cursorField: 'id',
		})
	}

	static buildCommentConnectionConfig(
		args: CommentQueryArgs,
		info?: any
	): ConnectionConfig {
		const where = {}
		const orderBy = 'sort' in args ? SortBuilder.buildCommentSort((args as any).sort) : undefined
		const include = info ? FieldSelection.buildCommentInclude(info) : COMMENT_INCLUDES
		
		return this.buildConfig({
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
			relationFields: ['post', 'author'],
			hasIdField: true,
			cursorField: 'id',
		})
	}
}

export class FilterBuilder {
	/**
	 * Build Prisma where clause from GraphQL filter input dynamically
	 * This approach uses runtime reflection to map filter operations
	 */
	static buildFilter(filter: any): any {
		if (!filter || typeof filter !== 'object') return {}

		const where: any = {}

		for (const [field, value] of Object.entries(filter)) {
			if (field === 'AND' && Array.isArray(value)) {
				where.AND = value.map((f: any) => this.buildFilter(f))
			} else if (field === 'OR' && Array.isArray(value)) {
				where.OR = value.map((f: any) => this.buildFilter(f))
			} else if (value && typeof value === 'object') {
				// Map filter operations dynamically
				const fieldWhere: any = {}
				
				// Copy all valid operations from the filter value
				for (const [operation, operationValue] of Object.entries(value)) {
					if (operationValue !== undefined && operationValue !== null) {
						fieldWhere[operation] = operationValue
					}
				}
				
				if (Object.keys(fieldWhere).length > 0) {
					where[field] = fieldWhere
				}
			}
		}

		return where
	}

	
	static buildUserFilter(filter?: UserFilterInput): any {
		return this.buildFilter(filter)
	}

	static buildPostFilter(filter?: PostFilterInput): any {
		return this.buildFilter(filter)
	}

	static buildCategoryFilter(filter?: CategoryFilterInput): any {
		return this.buildFilter(filter)
	}


}

export class SortBuilder {
	/**
	 * Build Prisma orderBy clause from GraphQL sort input dynamically
	 * This approach uses runtime reflection to map sort fields
	 */
	static buildSort(sort: any, fallbackSort: any = { id: 'asc' }): any {
		if (!sort || typeof sort !== 'object') return fallbackSort

		const orderBy: any = {}

		for (const [field, direction] of Object.entries(sort)) {
			if (direction && typeof direction === 'string') {
				// Convert enum values to lowercase for Prisma
				orderBy[field] = direction.toLowerCase()
			}
		}

		return Object.keys(orderBy).length > 0 ? orderBy : fallbackSort
	}

	
	static buildUserSort(sort?: UserSortInput): any {
		// For models without id field (like composite key models), don't use id as fallback
		const fallbackSort = true ? { id: 'asc' } : {}
		return this.buildSort(sort, fallbackSort)
	}

	static buildPostSort(sort?: PostSortInput): any {
		// For models without id field (like composite key models), don't use id as fallback
		const fallbackSort = true ? { id: 'asc' } : {}
		return this.buildSort(sort, fallbackSort)
	}



	static buildCommentSort(sort?: CommentSortInput): any {
		// For models without id field (like composite key models), don't use id as fallback
		const fallbackSort = true ? { id: 'asc' } : {}
		return this.buildSort(sort, fallbackSort)
	}
}

// Simplified field selection utilities (GraphQL-import-free)
// Uses static includes instead of dynamic GraphQL field parsing

export interface ResolveTree {
	name: string
	alias: string
	args: { [str: string]: unknown }
	fieldsByTypeName: FieldsByTypeName
}

export interface FieldsByTypeName {
	[str: string]: { [str: string]: ResolveTree }
}

// Simplified version that doesn't require GraphQL imports
export function buildPrismaInclude(_resolveInfo: any, relations: string[] = []): any {
	// For now, return a simple include object based on available relations
	// This avoids GraphQL module conflicts while maintaining basic functionality
	const include: any = {}
	
	relations.forEach(relation => {
		include[relation] = true
	})
	
	return include
}

export class FieldSelection {
	
	static buildUserInclude(info?: any): any {
		const relationFields = ['posts', 'comments']
		return buildPrismaInclude(info, relationFields)
	}

	static buildPostInclude(info?: any): any {
		const relationFields = ['author', 'categories', 'comments']
		return buildPrismaInclude(info, relationFields)
	}

	static buildCategoryInclude(info?: any): any {
		const relationFields = ['posts']
		return buildPrismaInclude(info, relationFields)
	}

	static buildPostCategoryInclude(info?: any): any {
		const relationFields = ['post', 'category']
		return buildPrismaInclude(info, relationFields)
	}

	static buildCommentInclude(info?: any): any {
		const relationFields = ['post', 'author']
		return buildPrismaInclude(info, relationFields)
	}
}


export const USER_INCLUDES = {
	posts: true,
	comments: true
}

export const POST_INCLUDES = {
	author: true,
	categories: true,
	comments: true
}

export const CATEGORY_INCLUDES = {
	posts: true
}

export const POSTCATEGORY_INCLUDES = {
	post: true,
	category: true
}

export const COMMENT_INCLUDES = {
	post: true,
	author: true
}