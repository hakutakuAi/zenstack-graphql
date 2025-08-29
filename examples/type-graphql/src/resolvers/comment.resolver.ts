import { Resolver, Query, Mutation, Arg, Ctx, FieldResolver, Root, Int, Info } from 'type-graphql'
import type { GraphQLResolveInfo } from 'graphql'
import { Comment, User, Post, CommentFilterInput, CommentSortInput, CommentConnection } from '../../schema'
import type { Context } from './types'
import { buildConnection, buildFilter, buildSort, buildSelect } from '@hakutakuai/zenstack-graphql/helpers'
import { COMMENT_WITH_RELATIONS_SELECT, USER_WITH_ALL_RELATIONS_SELECT, POST_WITH_ALL_RELATIONS_SELECT } from '../select-definitions'

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
		const where = buildFilter(filter as any)
		const orderBy = buildSort(sort as any)
		const select = buildSelect(COMMENT_WITH_RELATIONS_SELECT, info)

		const config = buildConnection({
			first,
			after,
			last,
			before,
			where,
			orderBy,
			select,
		})

		const [items, totalCount] = await Promise.all([prisma.comment.findMany(config.findMany), prisma.comment.count(config.count)])

		return config.toConnection(items, totalCount) as CommentConnection
	}

	@Mutation(() => Comment)
	async createComment(
		@Arg('content', () => String) content: string,
		@Arg('postId', () => String) postId: string,
		@Arg('authorId', () => String) authorId: string,
		@Ctx() { prisma }: Context,
	): Promise<Comment> {
		return (await prisma.comment.create({ data: { content, postId, authorId }, select: COMMENT_WITH_RELATIONS_SELECT })) as Comment
	}

	@FieldResolver(() => User)
	async author(@Root() comment: Comment, @Ctx() { prisma }: Context): Promise<User | null> {
		return (await prisma.user.findUnique({ where: { id: comment.authorId }, select: USER_WITH_ALL_RELATIONS_SELECT })) as User | null
	}

	@FieldResolver(() => Post)
	async post(@Root() comment: Comment, @Ctx() { prisma }: Context): Promise<Post | null> {
		return (await prisma.post.findUnique({ where: { id: comment.postId }, select: POST_WITH_ALL_RELATIONS_SELECT })) as Post | null
	}
}
