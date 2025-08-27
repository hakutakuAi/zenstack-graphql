import type { GraphQLResolveInfo } from 'graphql'
import { Member, MemberQueryArgs, MemberConnection, MemberFilterInput, MemberSortInput, UserProfile, UserProfileQueryArgs, UserProfileConnection, UserProfileFilterInput, UserProfileSortInput, Post, PostQueryArgs, PostConnection, PostFilterInput, PostSortInput, Comment, CommentQueryArgs, CommentConnection, CommentFilterInput, CommentSortInput, Reaction, ReactionQueryArgs, ReactionConnection, ReactionFilterInput, ReactionSortInput, Follow, FollowQueryArgs, FollowConnection, FollowFilterInput, FollowSortInput, Tag, TagQueryArgs, TagConnection, TagFilterInput, TagSortInput, TagsOnPosts, TagsOnPostsQueryArgs, TagsOnPostsConnection, TagsOnPostsFilterInput } from './schema'

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


	
	static buildMemberConnectionConfig(
		args: MemberQueryArgs,
		info?: any
	): ConnectionConfig {
		const where = 'filter' in args ? FilterBuilder.buildMemberFilter((args as any).filter) : {}
		const orderBy = 'sort' in args ? SortBuilder.buildMemberSort((args as any).sort) : undefined
		const include = info ? FieldSelection.buildMemberInclude(info) : MEMBER_INCLUDES
		
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
			relationFields: ['posts', 'comments', 'follows', 'followers', 'likes', 'profile'],
			hasIdField: true,
			cursorField: 'id',
		})
	}

	static buildUserProfileConnectionConfig(
		args: UserProfileQueryArgs,
		info?: any
	): ConnectionConfig {
		const where = 'filter' in args ? FilterBuilder.buildUserProfileFilter((args as any).filter) : {}
		const orderBy = 'sort' in args ? SortBuilder.buildUserProfileSort((args as any).sort) : undefined
		const include = info ? FieldSelection.buildUserProfileInclude(info) : USERPROFILE_INCLUDES
		
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
			relationFields: ['user'],
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
			relationFields: ['author', 'comments', 'likes', 'tags'],
			hasIdField: true,
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
			relationFields: ['author', 'post', 'parent', 'replies', 'likes'],
			hasIdField: true,
			cursorField: 'id',
		})
	}

	static buildReactionConnectionConfig(
		args: ReactionQueryArgs,
		info?: any
	): ConnectionConfig {
		const where = {}
		const orderBy = 'sort' in args ? SortBuilder.buildReactionSort((args as any).sort) : undefined
		const include = info ? FieldSelection.buildReactionInclude(info) : REACTION_INCLUDES
		
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
			relationFields: ['user', 'post', 'comment'],
			hasIdField: true,
			cursorField: 'id',
		})
	}

	static buildFollowConnectionConfig(
		args: FollowQueryArgs,
		info?: any
	): ConnectionConfig {
		const where = 'filter' in args ? FilterBuilder.buildFollowFilter((args as any).filter) : {}
		const orderBy = 'sort' in args ? SortBuilder.buildFollowSort((args as any).sort) : undefined
		const include = info ? FieldSelection.buildFollowInclude(info) : FOLLOW_INCLUDES
		
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
			relationFields: ['follower', 'following'],
			hasIdField: true,
			cursorField: 'id',
		})
	}

	static buildTagConnectionConfig(
		args: TagQueryArgs,
		info?: any
	): ConnectionConfig {
		const where = 'filter' in args ? FilterBuilder.buildTagFilter((args as any).filter) : {}
		const orderBy = 'sort' in args ? SortBuilder.buildTagSort((args as any).sort) : undefined
		const include = info ? FieldSelection.buildTagInclude(info) : TAG_INCLUDES
		
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

	static buildTagsOnPostsConnectionConfig(
		args: TagsOnPostsQueryArgs,
		info?: any
	): ConnectionConfig {
		const where = {}
		const orderBy = undefined
		const include = info ? FieldSelection.buildTagsOnPostsInclude(info) : TAGSONPOSTS_INCLUDES
		
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
			relationFields: ['post', 'tag'],
			hasIdField: false,
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

	
	static buildMemberFilter(filter?: MemberFilterInput): any {
		return this.buildFilter(filter)
	}

	static buildUserProfileFilter(filter?: UserProfileFilterInput): any {
		return this.buildFilter(filter)
	}

	static buildPostFilter(filter?: PostFilterInput): any {
		return this.buildFilter(filter)
	}

	static buildCommentFilter(filter?: CommentFilterInput): any {
		return this.buildFilter(filter)
	}


	static buildFollowFilter(filter?: FollowFilterInput): any {
		return this.buildFilter(filter)
	}

	static buildTagFilter(filter?: TagFilterInput): any {
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

	
	static buildMemberSort(sort?: MemberSortInput): any {
		// For models without id field (like composite key models), don't use id as fallback
		const fallbackSort = true ? { id: 'asc' } : {}
		return this.buildSort(sort, fallbackSort)
	}

	static buildUserProfileSort(sort?: UserProfileSortInput): any {
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

	static buildReactionSort(sort?: ReactionSortInput): any {
		// For models without id field (like composite key models), don't use id as fallback
		const fallbackSort = true ? { id: 'asc' } : {}
		return this.buildSort(sort, fallbackSort)
	}

	static buildFollowSort(sort?: FollowSortInput): any {
		// For models without id field (like composite key models), don't use id as fallback
		const fallbackSort = true ? { id: 'asc' } : {}
		return this.buildSort(sort, fallbackSort)
	}

	static buildTagSort(sort?: TagSortInput): any {
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
	
	static buildMemberInclude(info?: any): any {
		const relationFields = ['posts', 'comments', 'follows', 'followers', 'likes', 'profile']
		return buildPrismaInclude(info, relationFields)
	}

	static buildUserProfileInclude(info?: any): any {
		const relationFields = ['user']
		return buildPrismaInclude(info, relationFields)
	}

	static buildPostInclude(info?: any): any {
		const relationFields = ['author', 'comments', 'likes', 'tags']
		return buildPrismaInclude(info, relationFields)
	}

	static buildCommentInclude(info?: any): any {
		const relationFields = ['author', 'post', 'parent', 'replies', 'likes']
		return buildPrismaInclude(info, relationFields)
	}

	static buildReactionInclude(info?: any): any {
		const relationFields = ['user', 'post', 'comment']
		return buildPrismaInclude(info, relationFields)
	}

	static buildFollowInclude(info?: any): any {
		const relationFields = ['follower', 'following']
		return buildPrismaInclude(info, relationFields)
	}

	static buildTagInclude(info?: any): any {
		const relationFields = ['posts']
		return buildPrismaInclude(info, relationFields)
	}

	static buildTagsOnPostsInclude(info?: any): any {
		const relationFields = ['post', 'tag']
		return buildPrismaInclude(info, relationFields)
	}
}


export const MEMBER_INCLUDES = {
	posts: true,
	comments: true,
	follows: true,
	followers: true,
	likes: true,
	profile: true
}

export const USERPROFILE_INCLUDES = {
	user: true
}

export const POST_INCLUDES = {
	author: true,
	comments: true,
	likes: true,
	tags: true
}

export const COMMENT_INCLUDES = {
	author: true,
	post: true,
	parent: true,
	replies: true,
	likes: true
}

export const REACTION_INCLUDES = {
	user: true,
	post: true,
	comment: true
}

export const FOLLOW_INCLUDES = {
	follower: true,
	following: true
}

export const TAG_INCLUDES = {
	posts: true
}

export const TAGSONPOSTS_INCLUDES = {
	post: true,
	tag: true
}