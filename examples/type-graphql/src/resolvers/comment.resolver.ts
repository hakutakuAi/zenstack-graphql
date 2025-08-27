import { Resolver, Query, Mutation, Arg, Ctx, FieldResolver, Root, Int, Info } from 'type-graphql'
import type { GraphQLResolveInfo } from 'graphql'
import { Comment, User, Post, CommentFilterInput, CommentSortInput, CommentConnection, CommentQueryArgs } from '../../schema'
import { BaseResolver } from './base-resolver'
import type { Context } from './types'
import { ConnectionBuilder, POST_INCLUDES, USER_INCLUDES, COMMENT_INCLUDES } from '../../schema-helpers'

@Resolver(() => Comment)
export class CommentResolver extends BaseResolver {
	@Query(() => CommentConnection)
	async comments(
		@Arg('filter', () => CommentFilterInput, { nullable: true }) filter: CommentFilterInput | undefined,
		@Arg('sort', () => CommentSortInput, { nullable: true }) sort: CommentSortInput | undefined,
		@Arg('first', () => Int, { nullable: true }) first: number | undefined,
		@Arg('after', () => String, { nullable: true }) after: string | undefined,
		@Arg('last', () => Int, { nullable: true }) last: number | undefined,
		@Arg('before', () => String, { nullable: true }) before: string | undefined,
		@Info() info: GraphQLResolveInfo,
		@Ctx() { prisma }: Context,
	): Promise<CommentConnection> {
		const args: CommentQueryArgs = { filter, sort, first, after, last, before }
		return ConnectionBuilder.buildCommentConnection(prisma, args, info)
	}

	@Mutation(() => Comment)
	async createComment(
		@Arg('content', () => String) content: string,
		@Arg('postId', () => String) postId: string,
		@Arg('authorId', () => String) authorId: string,
		@Ctx() { prisma }: Context,
	): Promise<Comment> {
		return this.create<Comment>(prisma, 'comment', { content, postId, authorId }, COMMENT_INCLUDES)
	}

	@FieldResolver(() => User)
	async author(@Root() comment: Comment, @Ctx() { prisma }: Context): Promise<User | null> {
		return this.findUnique<User>(prisma, 'user', { id: comment.authorId }, USER_INCLUDES)
	}

	@FieldResolver(() => Post)
	async post(@Root() comment: Comment, @Ctx() { prisma }: Context): Promise<Post | null> {
		return this.findUnique<Post>(prisma, 'post', { id: comment.postId }, POST_INCLUDES)
	}
}
