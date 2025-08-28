import { Resolver, Query, Mutation, Arg, Ctx, FieldResolver, Root, Int, Info } from 'type-graphql'
import type { GraphQLResolveInfo } from 'graphql'
import { Comment, User, Post, CommentFilterInput, CommentSortInput, CommentConnection, CommentQueryArgs } from '../../schema'
import type { Context } from './types'
import { ConnectionBuilder, POST_INCLUDES, USER_INCLUDES, COMMENT_INCLUDES } from '../../schema-helpers'

@Resolver(() => Comment)
export class CommentResolver {
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
		const config = ConnectionBuilder.buildCommentConnectionConfig(args, info)

		const items = await prisma.comment.findMany(config.findManyOptions)
		const totalCount = await prisma.comment.count(config.countOptions)

		return ConnectionBuilder.processResults(items, totalCount, config.paginationInfo) as CommentConnection
	}

	@Mutation(() => Comment)
	async createComment(
		@Arg('content', () => String) content: string,
		@Arg('postId', () => String) postId: string,
		@Arg('authorId', () => String) authorId: string,
		@Ctx() { prisma }: Context,
	): Promise<Comment> {
		return (await prisma.comment.create({ data: { content, postId, authorId }, include: COMMENT_INCLUDES })) as Comment
	}

	@FieldResolver(() => User)
	async author(@Root() comment: Comment, @Ctx() { prisma }: Context): Promise<User | null> {
		return (await prisma.user.findUnique({ where: { id: comment.authorId }, include: USER_INCLUDES })) as User | null
	}

	@FieldResolver(() => Post)
	async post(@Root() comment: Comment, @Ctx() { prisma }: Context): Promise<Post | null> {
		return (await prisma.post.findUnique({ where: { id: comment.postId }, include: POST_INCLUDES })) as Post | null
	}
}
