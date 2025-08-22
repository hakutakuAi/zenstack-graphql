import { Resolver, Query, Mutation, Arg, Ctx, FieldResolver, Root, Int } from 'type-graphql'
import { Comment, User, Post, CommentFilterInput, CommentSortInput, CommentConnection } from '../../schema'
import { BaseResolver } from './base-resolver'
import { Context } from './types'
import { PRISMA_INCLUDES } from '../utils/resolver-helpers'

@Resolver(() => Comment)
export class CommentResolver extends BaseResolver {
	private readonly ALLOWED_FILTER_FIELDS: string[] = []
	private readonly ALLOWED_SORT_FIELDS = ['createdAt']

	@Query(() => CommentConnection)
	async comments(
		@Arg('filter', () => CommentFilterInput, { nullable: true }) filter: CommentFilterInput | null,
		@Arg('sort', () => CommentSortInput, { nullable: true }) sort: CommentSortInput | null,
		@Arg('first', () => Int, { nullable: true }) first: number | null,
		@Arg('after', () => String, { nullable: true }) after: string | null,
		@Arg('last', () => Int, { nullable: true }) last: number | null,
		@Arg('before', () => String, { nullable: true }) before: string | null,
		@Ctx() { prisma }: Context,
	): Promise<CommentConnection> {
		return this.buildRelayConnection<Comment>(
			prisma,
			'comment',
			{ filter, sort, first, after, last, before },
			this.ALLOWED_FILTER_FIELDS,
			this.ALLOWED_SORT_FIELDS,
			PRISMA_INCLUDES.COMMENT,
		)
	}

	@Mutation(() => Comment)
	async createComment(
		@Arg('content', () => String) content: string,
		@Arg('postId', () => String) postId: string,
		@Arg('authorId', () => String) authorId: string,
		@Ctx() { prisma }: Context,
	): Promise<Comment> {
		return this.create<Comment>(prisma, 'comment', { content, postId, authorId }, PRISMA_INCLUDES.COMMENT)
	}

	@FieldResolver(() => User)
	async author(@Root() comment: Comment, @Ctx() { prisma }: Context): Promise<User | null> {
		return this.findUnique<User>(prisma, 'user', { id: comment.authorId })
	}

	@FieldResolver(() => Post)
	async post(@Root() comment: Comment, @Ctx() { prisma }: Context): Promise<Post | null> {
		return this.findUnique<Post>(prisma, 'post', { id: comment.postId }, PRISMA_INCLUDES.POST)
	}
}
