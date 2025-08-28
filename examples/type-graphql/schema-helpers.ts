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

		let take = first || last || 10
		if (last) take = -take

		const cursor = (hasIdField && (after || before)) 
			? { [cursorField]: (after || before)! } 
			: undefined
		const skip = cursor ? 1 : 0

		const finalInclude = info ? buildPrismaInclude(info, relationFields) : include

		const findManyOptions: any = {
			take: Math.abs(take) + 1, // Get one extra to check for next page
			where,
			orderBy,
			include: finalInclude,
		}

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

	static processResults<T>(
		items: T[],
		totalCount: number,
		paginationInfo: ConnectionConfig['paginationInfo']
	): ConnectionResult<T> {
		const { first, last, cursorField, hasIdField } = paginationInfo

		const hasNextPage = first ? items.length > first : false
		const hasPreviousPage = last ? items.length > Math.abs(last) : false

		const resultItems = hasNextPage || hasPreviousPage ? items.slice(0, -1) : items

		const edges = resultItems.map((item: any, index: number) => {
			let cursor: string
			if (hasIdField && item[cursorField]) {
				cursor = item[cursorField]
			} else {
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
		const where = 'filter' in args ? FilterBuilder.buildPostCategoryFilter((args as any).filter) : {}
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
		const where = 'filter' in args ? FilterBuilder.buildCommentFilter((args as any).filter) : {}
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
	static buildFilter(filter: any): any {
		if (!filter || typeof filter !== 'object') return {}

		const where: any = {}

		for (const [field, value] of Object.entries(filter)) {
			if (field === 'AND' && Array.isArray(value)) {
				where.AND = value.map((f: any) => this.buildFilter(f))
			} else if (field === 'OR' && Array.isArray(value)) {
				where.OR = value.map((f: any) => this.buildFilter(f))
			} else if (value && typeof value === 'object') {
				const fieldWhere: any = {}
				
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

	static buildPostCategoryFilter(filter?: PostCategoryFilterInput): any {
		return this.buildFilter(filter)
	}

	static buildCommentFilter(filter?: CommentFilterInput): any {
		return this.buildFilter(filter)
	}
}

export class SortBuilder {
	static buildSort(sort: any, fallbackSort: any = { id: 'asc' }): any {
		if (!sort || typeof sort !== 'object') return fallbackSort

		const orderBy: any = {}

		for (const [field, direction] of Object.entries(sort)) {
			if (direction && typeof direction === 'string') {
					orderBy[field] = direction.toLowerCase()
			}
		}

		return Object.keys(orderBy).length > 0 ? orderBy : fallbackSort
	}

	
	static buildUserSort(sort?: UserSortInput): any {
		const fallbackSort = true ? { id: 'asc' } : {}
		return this.buildSort(sort, fallbackSort)
	}

	static buildPostSort(sort?: PostSortInput): any {
		const fallbackSort = true ? { id: 'asc' } : {}
		return this.buildSort(sort, fallbackSort)
	}



	static buildCommentSort(sort?: CommentSortInput): any {
		const fallbackSort = true ? { id: 'asc' } : {}
		return this.buildSort(sort, fallbackSort)
	}
}



export interface ResolveTree {
	name: string
	alias: string
	args: { [str: string]: unknown }
	fieldsByTypeName: FieldsByTypeName
}

export interface FieldsByTypeName {
	[str: string]: { [str: string]: ResolveTree }
}

export function buildPrismaInclude(_resolveInfo: any, relations: string[] = []): any {
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